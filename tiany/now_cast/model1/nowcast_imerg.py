#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Train a simple ConvLSTM nowcasting model to predict M future hourly precipitation frames
from N past days of data.

Data assumptions:
- Input files are IMERG HDF5 or NetCDF containing /Grid/precipitation (mm/hr) on a global grid
  with dims (time, lon, lat) or (time, lat, lon). We transpose to (time, lat, lon).
- Half-hourly data are resampled to hourly mean.
- We recommend coarsening to reduce memory (e.g., 0.1° -> 1.0° using 10x10 mean coarsen).

Usage examples:
  # Train on a day’s worth of half-hourly files and coarsen to 1 degree
  python nowcast_imerg.py train \
      --data-glob "imerg/2025/*.HDF5" \
      --engine h5netcdf --group Grid \
      --coarsen-lat 10 --coarsen-lon 10 \
      --n-days 3 --m-hours 6 \
      --epochs 5 --batch-size 2 \
      --outdir runs/run_2025-08-31

  # Forecast using the last N days from files on disk
  python nowcast_imerg.py forecast \
      --data-glob "imerg/2025/*.HDF5" \
      --engine h5netcdf --group Grid \
      --coarsen-lat 10 --coarsen-lon 10 \
      --n-days 3 --m-hours 6 \
      --ckpt runs/run_2025-08-31/best.pt \
      --out-png forecast_example.png
"""

import os
import glob
import math
import argparse
from typing import List, Optional, Tuple

import numpy as np
import xarray as xr
import dask.array as da

import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
from tqdm import tqdm
import matplotlib.pyplot as plt


# -----------------------------
# Data utilities
# -----------------------------

def open_imerg_as_hourly(
    data_glob: str,
    engine: str = "h5netcdf",
    group: Optional[str] = "Grid",
    var_name: str = "precipitation",
    coarsen_lat: int = 10,
    coarsen_lon: int = 10,
    clip_nonpos_to: float = 0.0,
) -> xr.DataArray:
    """
    Open multiple IMERG files, extract precip (mm/hr), resample to hourly mean if needed,
    coarsen spatially, and return as time-lat-lon DataArray (float32), with NaNs for missing.

    Parameters
    ----------
    data_glob : str
        Glob like "imerg/2025/*.HDF5" or "imerg/*.nc"
    engine : str
        "h5netcdf" for HDF5 with groups, or "netcdf4", etc.
    group : str or None
        Set to "Grid" for IMERG HDF5. Use None for plain NetCDF without groups.
    var_name : str
        Variable name; IMERG uses "precipitation".
    coarsen_lat, coarsen_lon : int
        Coarsening factors (0 or 1 means no coarsen). For 0.1°->1° use 10,10.
    clip_nonpos_to : float
        Replace negative values (and optional <=0 for log) with this.

    Returns
    -------
    xr.DataArray (time, lat, lon) float32
    """
    files = sorted(glob.glob(data_glob))
    if not files:
        raise FileNotFoundError(f"No files match: {data_glob}")

    # xarray open_mfdataset supports group (h5netcdf) and will combine by coords
    open_kwargs = dict(engine=engine, combine="by_coords")
    if group is not None:
        open_kwargs["group"] = group

    ds = xr.open_mfdataset(files, **open_kwargs)

    if var_name not in ds:
        raise KeyError(f"Variable '{var_name}' not found. Available: {list(ds.data_vars)}")

    da_precip = ds[var_name]

    # Ensure dimension order (time, lat, lon)
    # IMERG is often (time, lon, lat)
    dims = list(da_precip.dims)
    if set(dims) != {"time", "lat", "lon"}:
        # Try to reorder
        order = [d for d in ["time", "lat", "lon"] if d in dims]
        da_precip = da_precip.transpose(*order)

    # Mask fill and ensure float32
    fv = da_precip.attrs.get("_FillValue", None)
    if fv is not None:
        da_precip = da_precip.where(da_precip != fv)

    da_precip = da_precip.astype("float32")

    # Replace negatives and NaNs
    da_precip = da_precip.where(np.isfinite(da_precip))
    if clip_nonpos_to is not None:
        da_precip = da_precip.where(da_precip > 0, clip_nonpos_to)

    # If half-hourly, resample to hourly mean
    # We check if there are multiple samples per hour via time diffs
    if da_precip.time.size >= 2:
        dt = (da_precip.time[1] - da_precip.time[0]).values
        # Heuristic: if < 3600 seconds → resample hourly
        try:
            seconds = np.abs(np.datetime64(dt, "s").astype("int64"))
        except Exception:
            seconds = 0
        # resample to hourly if needed
        #if seconds and seconds < 3600:
        #    da_precip = da_precip.resample(time="1H").mean(skipna=True)

    # Coarsen for memory; drop incomplete edges
    if coarsen_lat and coarsen_lat > 1:
        da_precip = da_precip.coarsen(lat=coarsen_lat, boundary="trim").mean()
    if coarsen_lon and coarsen_lon > 1:
        da_precip = da_precip.coarsen(lon=coarsen_lon, boundary="trim").mean()

    # Ensure regular order and contiguous
    da_precip = da_precip.transpose("time", "lat", "lon")

    return da_precip


def log1p_transform(x: np.ndarray) -> np.ndarray:
    # Handle negatives already clipped; log1p stabilizes variance
    return np.log1p(x)


def inv_log1p_transform(x: np.ndarray) -> np.ndarray:
    return np.expm1(x)


def make_windows(
    arr: np.ndarray, t_in: int, t_out: int, stride: int = 1
) -> Tuple[np.ndarray, np.ndarray]:
    """
    Create sliding windows from a 3D array (time, H, W) -> X: (N, t_in, H, W), Y: (N, t_out, H, W)
    """
    T, H, W = arr.shape
    n = (T - (t_in + t_out)) // stride + 1
    if n <= 0:
        raise ValueError("Not enough time steps for the requested windows.")
    xs = []
    ys = []
    for i in range(0, T - (t_in + t_out) + 1, stride):
        xs.append(arr[i : i + t_in])
        ys.append(arr[i + t_in : i + t_in + t_out])
    X = np.stack(xs, axis=0)
    Y = np.stack(ys, axis=0)
    return X, Y


class NowcastDataset(Dataset):
    def __init__(self, data_glob, engine, group, n_days, m_hours,
                 coarsen_lat, coarsen_lon, stride=1, split="train", split_frac=0.8):
        """
        Loads/prepare data once; splits into train/val chronologically.
        """
        da = open_imerg_as_hourly(
            data_glob=data_glob,
            engine=engine,
            group=group,
            coarsen_lat=coarsen_lat,
            coarsen_lon=coarsen_lon,
        )
        # Convert to numpy; for large data consider .chunk and on-the-fly loading instead.
        arr = da.values  # (T, H, W)
        # Transform
        arr = np.nan_to_num(arr, nan=0.0, posinf=0.0, neginf=0.0).astype(np.float32)
        arr = log1p_transform(arr)

        #t_in = int(n_days * 24)       # hourly input steps
        t_in = int(n_days * 48)       # half-hourly input steps
        #t_out = int(m_hours)          # hourly forecast steps
        t_out = int(m_hours*2)          # half-hourly forecast steps

        X, Y = make_windows(arr, t_in=t_in, t_out=t_out, stride=stride)
        # Chronological split
        n = X.shape[0]
        pivot = int(n * split_frac)
        if split == "train":
            self.X = X[:pivot]
            self.Y = Y[:pivot]
        else:
            self.X = X[pivot:]
            self.Y = Y[pivot:]

    def __len__(self):
        return self.X.shape[0]

    def __getitem__(self, idx):
        # Return tensors shaped (T, C, H, W) with C=1
        x = torch.from_numpy(self.X[idx]).unsqueeze(1)  # (T_in, 1, H, W)
        y = torch.from_numpy(self.Y[idx]).unsqueeze(1)  # (T_out, 1, H, W)
        return x, y


# -----------------------------
# ConvLSTM model
# -----------------------------

class ConvLSTMCell(nn.Module):
    def __init__(self, input_channels, hidden_channels, kernel_size=3):
        super().__init__()
        padding = kernel_size // 2
        self.input_channels = input_channels
        self.hidden_channels = hidden_channels
        self.conv = nn.Conv2d(
            input_channels + hidden_channels, 4 * hidden_channels,
            kernel_size=kernel_size, padding=padding
        )

    def forward(self, x, h, c):
        # x: (B, C, H, W), h,c: (B, Hc, H, W)
        combined = torch.cat([x, h], dim=1)
        gates = self.conv(combined)
        (i, f, o, g) = torch.chunk(gates, 4, dim=1)
        i = torch.sigmoid(i)
        f = torch.sigmoid(f)
        o = torch.sigmoid(o)
        g = torch.tanh(g)
        c = f * c + i * g
        h = o * torch.tanh(c)
        return h, c

    def init_state(self, batch, spatial):
        H, W = spatial
        device = next(self.parameters()).device
        h = torch.zeros(batch, self.hidden_channels, H, W, device=device)
        c = torch.zeros(batch, self.hidden_channels, H, W, device=device)
        return h, c


class ConvLSTMStack(nn.Module):
    def __init__(self, channel_list, kernel_size=3):
        """
        channel_list: [in_C, h1, h2, ..., hL]
        """
        super().__init__()
        cells = []
        for i in range(len(channel_list) - 1):
            cells.append(ConvLSTMCell(channel_list[i], channel_list[i + 1], kernel_size))
        self.cells = nn.ModuleList(cells)

    def forward(self, x_seq):
        """
        x_seq: (B, T, C, H, W)
        Returns hidden states of last time for each layer.
        """
        B, T, C, H, W = x_seq.shape
        hs, cs = [], []
        # init states
        h_list, c_list = [], []
        for cell in self.cells:
            h, c = cell.init_state(B, (H, W))
            h_list.append(h); c_list.append(c)

        for t in range(T):
            x = x_seq[:, t]
            for l, cell in enumerate(self.cells):
                h, c = cell(x, h_list[l], c_list[l])
                h_list[l], c_list[l] = h, c
                x = h  # feed into next layer
        # return last states
        return h_list, c_list


class ConvLSTMForecaster(nn.Module):
    def __init__(self, in_channels=1, hidden=[32, 32], kernel_size=3, out_channels=1):
        super().__init__()
        self.encoder = ConvLSTMStack([in_channels] + hidden, kernel_size)
        self.decoder_cells = nn.ModuleList([
            ConvLSTMCell(hidden[-1], hidden[-1], kernel_size)
        ])
        self.head = nn.Conv2d(hidden[-1], out_channels, kernel_size=1)

    def forward(self, x_in, steps_out):
        """
        x_in: (B, T_in, 1, H, W)
        steps_out: int (T_out)
        Returns y_hat: (B, T_out, 1, H, W)
        """
        B, T_in, C, H, W = x_in.shape
        # Encode
        h_list, c_list = self.encoder(x_in)
        h, c = h_list[-1], c_list[-1]

        outputs = []
        for t in range(steps_out):
            x = h
            # One decoder ConvLSTM layer driven by previous output embedding
            h, c = self.decoder_cells[0](x, h, c)
            frame = self.head(h)
            outputs.append(frame)
            x = frame  # autoregressive
        y_hat = torch.stack(outputs, dim=1)
        return y_hat


# -----------------------------
# Training / evaluation
# -----------------------------

def masked_mse(y_hat, y_true):
    mask = torch.isfinite(y_true)
    y_hat = torch.where(mask, y_hat, torch.zeros_like(y_hat))
    y_true = torch.where(mask, y_true, torch.zeros_like(y_true))
    denom = mask.float().sum().clamp_min(1.0)
    return ((y_hat - y_true) ** 2 * mask).sum() / denom


def train_one_epoch(model, loader, optim, device):
    model.train()
    losses = []
    for x, y in tqdm(loader, desc="Train", leave=False):
        x = x.to(device)  # (B,T_in,1,H,W)
        y = y.to(device)  # (B,T_out,1,H,W)
        optim.zero_grad()
        y_hat = model(x, steps_out=y.shape[1])
        loss = masked_mse(y_hat, y)
        loss.backward()
        optim.step()
        losses.append(loss.item())
    return float(np.mean(losses))


@torch.no_grad()
def validate(model, loader, device):
    model.eval()
    losses = []
    for x, y in tqdm(loader, desc="Val", leave=False):
        x = x.to(device)
        y = y.to(device)
        y_hat = model(x, steps_out=y.shape[1])
        loss = masked_mse(y_hat, y)
        losses.append(loss.item())
    return float(np.mean(losses))


def plot_forecast_sample(y_true, y_hat, out_png=None):
    """
    Plot a quick panel for the first sample in batch: last input, and first 3 target/pred frames.
    """
    b = 0
    yt = y_true[b].cpu().numpy()  # (T_out, 1, H, W)
    yh = y_hat[b].detach().cpu().numpy()
    tshow = min(3, yt.shape[0])
    vmax = float(np.percentile(np.expm1(yt), 99))  # in mm/hr scale

    fig, axes = plt.subplots(2, tshow, figsize=(4*tshow, 6))
    for i in range(tshow):
        im0 = axes[0, i].imshow(np.expm1(yt[i, 0]), cmap="turbo", vmin=0, vmax=vmax)
        axes[0, i].set_title(f"Truth t+{i+1}h")
        fig.colorbar(im0, ax=axes[0, i], fraction=0.046, pad=0.04)
        im1 = axes[1, i].imshow(np.expm1(yh[i, 0]), cmap="turbo", vmin=0, vmax=vmax)
        axes[1, i].set_title(f"Pred t+{i+1}h")
        fig.colorbar(im1, ax=axes[1, i], fraction=0.046, pad=0.04)
    for ax in axes.ravel(): ax.axis("off")
    fig.tight_layout()
    if out_png:
        fig.savefig(out_png, dpi=150)
    plt.close(fig)


def main_train(args):
    device = torch.device("cuda" if torch.cuda.is_available() and not args.cpu else "cpu")

    train_set = NowcastDataset(
        data_glob=args.data_glob, engine=args.engine, group=args.group,
        n_days=args.n_days, m_hours=args.m_hours,
        coarsen_lat=args.coarsen_lat, coarsen_lon=args.coarsen_lon,
        stride=args.stride, split="train", split_frac=args.split_frac
    )
    val_set = NowcastDataset(
        data_glob=args.data_glob, engine=args.engine, group=args.group,
        n_days=args.n_days, m_hours=args.m_hours,
        coarsen_lat=args.coarsen_lat, coarsen_lon=args.coarsen_lon,
        stride=args.stride, split="val", split_frac=args.split_frac
    )

    train_loader = DataLoader(train_set, batch_size=args.batch_size, shuffle=True, num_workers=2, drop_last=True)
    val_loader   = DataLoader(val_set,   batch_size=args.batch_size, shuffle=False, num_workers=2)

    # Model
    model = ConvLSTMForecaster(
        in_channels=1, hidden=[args.hidden, args.hidden],
        kernel_size=3, out_channels=1
    ).to(device)

    optim = torch.optim.Adam(model.parameters(), lr=args.lr)
    os.makedirs(args.outdir, exist_ok=True)
    best_val = math.inf
    ckpt_path = os.path.join(args.outdir, "best.pt")

    for epoch in range(1, args.epochs + 1):
        tr = train_one_epoch(model, train_loader, optim, device)
        va = validate(model, val_loader, device)
        print(f"Epoch {epoch:03d} | train MSE={tr:.5f} | val MSE={va:.5f}")

        # Save best
        if va < best_val:
            best_val = va
            torch.save({"model": model.state_dict(), "args": vars(args)}, ckpt_path)
            # Produce a quick sample figure
            x, y = next(iter(val_loader))
            x = x.to(device); y = y.to(device)
            y_hat = model(x, steps_out=y.shape[1])
            plot_forecast_sample(y, y_hat, out_png=os.path.join(args.outdir, "val_sample.png"))

    print(f"Best val MSE: {best_val:.5f}. Checkpoint: {ckpt_path}")


@torch.no_grad()
def main_forecast(args):
    device = torch.device("cuda" if torch.cuda.is_available() and not args.cpu else "cpu")

    # Build just one dataset to grasp grid and to take the last N days window
    da = open_imerg_as_hourly(
        data_glob=args.data_glob, engine=args.engine, group=args.group,
        coarsen_lat=args.coarsen_lat, coarsen_lon=args.coarsen_lon,
    )
    arr = da.values  # (T, H, W)
    arr = np.nan_to_num(arr, nan=0.0, posinf=0.0, neginf=0.0).astype(np.float32)
    arr = log1p_transform(arr)

    #t_in = int(args.n_days * 24)   # hourly
    t_in = int(args.n_days * 48)   # half-hourly
    if arr.shape[0] < t_in:
        raise ValueError(f"Not enough data to extract last {t_in} hours for input.")
    x_in = arr[-t_in:]  # (T_in, H, W)

    # Load model
    chk = torch.load(args.ckpt, map_location=device)
    model = ConvLSTMForecaster(
        in_channels=1, hidden=[args.hidden, args.hidden], kernel_size=3, out_channels=1
    ).to(device)
    model.load_state_dict(chk["model"])
    model.eval()

    x = torch.from_numpy(x_in).unsqueeze(0).unsqueeze(2).to(device)  # (1, T_in, 1, H, W)
    t_out=args.m_hours * 2
    y_hat = model(x, steps_out=t_out)  # (1, T_out, 1, H, W)

    # Save a quick panel
    if args.out_png:
        # Make a dummy y_true zeros just for panel layout
        y_true = torch.zeros_like(y_hat)
        plot_forecast_sample(y_true, y_hat, out_png=args.out_png)

    # Optionally write to NetCDF
    if args.out_nc:
      y_np = y_hat[0, :, 0].detach().cpu().numpy()   # (T_out, H, W), log-space
      y_mmhr = inv_log1p_transform(y_np)

      # Last valid analysis time
      t0 = np.datetime64(da.time.values[-1], "ns")

      # Half-hourly forecast times (M hours × 2)
      t_out = args.m_hours * 2
      times = np.array(
          [t0 + np.timedelta64(30 * (i + 1), "m") for i in range(t_out)],
          dtype="datetime64[ns]"
      )

      da_out = xr.DataArray(
          y_mmhr.astype("float32"),
          dims=("time", "lat", "lon"),
          coords={
              "time": times,
              "lat": da.lat.values,
              "lon": da.lon.values,
          },
          name="precipitation_forecast",
          attrs={
              "units": "mm/hr",
              "description": "Half-hourly ConvLSTM precipitation forecast",
              "lead_time_units": "minutes",
              "lead_time_step": 30,
          }
      )

      out_ds = xr.Dataset({"precipitation_forecast": da_out})
      out_ds.to_netcdf(args.out_nc, encoding={
          "precipitation_forecast": {"zlib": True, "complevel": 4}
      })

      print(f"Wrote forecast NetCDF: {args.out_nc}")


def build_argparser():
    p = argparse.ArgumentParser(description="Short-term global precipitation nowcasting (ConvLSTM).")
    sub = p.add_subparsers(dest="cmd", required=True)

    def add_common(sp):
        sp.add_argument("--data-glob", required=True, help="Glob for input files (HDF5/NetCDF)")
        sp.add_argument("--engine", default="h5netcdf", help="xarray engine (h5netcdf|netcdf4|zarr)")
        sp.add_argument("--group", default="Grid", help="HDF5 group (IMERG uses 'Grid'). Use None for NetCDF", type=str)
        sp.add_argument("--coarsen-lat", type=int, default=10, help="Coarsen factor along lat (0/1=no coarsen)")
        sp.add_argument("--coarsen-lon", type=int, default=10, help="Coarsen factor along lon (0/1=no coarsen)")
        sp.add_argument("--n-days", type=int, default=3, help="Past N days to use")
        sp.add_argument("--m-hours", type=int, default=6, help="Future M hours to predict")
        sp.add_argument("--hidden", type=int, default=32, help="Hidden channels in ConvLSTM")
        sp.add_argument("--cpu", action="store_true", help="Force CPU even if CUDA available")

    sp_tr = sub.add_parser("train", help="Train the model")
    add_common(sp_tr)
    sp_tr.add_argument("--epochs", type=int, default=5)
    sp_tr.add_argument("--batch-size", type=int, default=2)
    sp_tr.add_argument("--lr", type=float, default=1e-3)
    sp_tr.add_argument("--stride", type=int, default=1, help="Stride of sliding windows")
    sp_tr.add_argument("--split-frac", type=float, default=0.8, help="Train fraction in chronological split")
    sp_tr.add_argument("--outdir", required=True)

    sp_fc = sub.add_parser("forecast", help="Run inference with trained checkpoint")
    add_common(sp_fc)
    sp_fc.add_argument("--ckpt", required=True, help="Path to best.pt")
    sp_fc.add_argument("--out-png", default=None, help="Save a quick panel figure")
    sp_fc.add_argument("--out-nc", default=None, help="Save forecast to NetCDF")

    return p


if __name__ == "__main__":
    args = build_argparser().parse_args()
    if args.group in ("None", "none", "null", ""):
        args.group = None
    if args.cmd == "train":
        main_train(args)
    elif args.cmd == "forecast":
        main_forecast(args)
