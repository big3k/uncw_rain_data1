#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Verification of half-hourly precipitation forecasts vs. IMERG truth.

Metrics computed per lead time:
- RMSE
- MAE
- Bias
- Correlation
Optional threshold-based metrics.

Author: Your Nowcasting System
"""

import argparse
import numpy as np
import xarray as xr
from pathlib import Path
import cftime

def to_cftime_julian(times):
    """Convert numpy datetime64 times to cftime.DatetimeJulian"""
    out = []
    for t in times:
        ts = np.datetime64(t).astype("datetime64[ns]")
        y = int(str(ts)[0:4])
        m = int(str(ts)[5:7])
        d = int(str(ts)[8:10])
        hh = int(str(ts)[11:13])
        mm = int(str(ts)[14:16])
        ss = int(str(ts)[17:19])
        out.append(cftime.DatetimeJulian(
            y, m, d, hh, mm, ss, has_year_zero=False
        ))
    return np.array(out, dtype=object)


# -------------------------
# Metric functions
# -------------------------

def rmse(f, o):
    return np.sqrt(np.nanmean((f - o) ** 2))


def mae(f, o):
    return np.nanmean(np.abs(f - o))


def bias(f, o):
    return np.nanmean(f - o)


def corr(f, o):
    f0 = f - np.nanmean(f)
    o0 = o - np.nanmean(o)
    return np.nanmean(f0 * o0) / (
        np.sqrt(np.nanmean(f0 ** 2)) * np.sqrt(np.nanmean(o0 ** 2)) + 1e-12
    )


def contingency_metrics(f, o, thresh):
    f_event = f >= thresh
    o_event = o >= thresh

    hits = np.logical_and(f_event, o_event).sum()
    misses = np.logical_and(~f_event, o_event).sum()
    false_alarms = np.logical_and(f_event, ~o_event).sum()

    pod = hits / (hits + misses + 1e-12)
    far = false_alarms / (hits + false_alarms + 1e-12)
    csi = hits / (hits + misses + false_alarms + 1e-12)

    return pod, far, csi


# -------------------------
# Data loading
# -------------------------

def load_forecast(nc_file):
    ds = xr.open_dataset(nc_file)
    da = ds["precipitation_forecast"]
    return da

def load_imerg_truth(
    times,
    imerg_glob,
    engine="h5netcdf",
    group="Grid",
    coarsen_lat=1,
    coarsen_lon=1,
):
    import xarray as xr
    import numpy as np
    import cftime

    ds = xr.open_mfdataset(
        imerg_glob,
        engine=engine,
        group=group,
        combine="by_coords"
    )

    da = ds["precipitation"].transpose("time", "lat", "lon")

    # --- calendar-safe time alignment (already fixed) ---
    times_cf = to_cftime_julian(times.values)
    da = da.sel(time=times_cf, method="nearest")

    if da.sizes["time"] == 0:
        raise RuntimeError("No IMERG data matched forecast times.")

    # --- NEW: coarsen IMERG truth to forecast grid ---
    if coarsen_lat > 1:
        da = da.coarsen(lat=coarsen_lat, boundary="trim").mean()

    if coarsen_lon > 1:
        da = da.coarsen(lon=coarsen_lon, boundary="trim").mean()

    return da

# -------------------------
# Main verification routine
# -------------------------

def verify(
    forecast_nc,
    imerg_glob,
    out_nc,
    thresholds=(0.1, 1.0, 5.0),
    coarsen_lat=1,
    coarsen_lon=1,
):
    fc = load_forecast(forecast_nc)
    obs = load_imerg_truth(
        fc.time,
        imerg_glob,
        coarsen_lat=coarsen_lat,
        coarsen_lon=coarsen_lon,
    )

    metrics = {
        "rmse": [],
        "mae": [],
        "bias": [],
        "corr": []
    }

    pod_dict = {t: [] for t in thresholds}
    far_dict = {t: [] for t in thresholds}
    csi_dict = {t: [] for t in thresholds}


    nT = fc.sizes["time"]

    for i in range(nT):
        f = fc.isel(time=i).values
        o = obs.isel(time=i).values

        mask = np.isfinite(f) & np.isfinite(o)
        f = f[mask]
        o = o[mask]

        metrics["rmse"].append(rmse(f, o))
        metrics["mae"].append(mae(f, o))
        metrics["bias"].append(bias(f, o))
        metrics["corr"].append(corr(f, o))

        for thr in thresholds:
            pod, far, csi = contingency_metrics(f, o, thr)
            pod_dict[thr].append(pod)
            far_dict[thr].append(far)
            csi_dict[thr].append(csi)

    # Build output Dataset
    lead_index = np.arange(len(fc.time)) * 0.5  # hours

    ds_out = xr.Dataset(
        {
            "rmse": ("lead_time", metrics["rmse"]),
            "mae": ("lead_time", metrics["mae"]),
            "bias": ("lead_time", metrics["bias"]),
            "corr": ("lead_time", metrics["corr"]),
        },
        coords={"lead_time": lead_index}
    )

    for thr in thresholds:
        ds_out[f"pod_{thr}"] = ("lead_time", pod_dict[thr])
        ds_out[f"far_{thr}"] = ("lead_time", far_dict[thr])
        ds_out[f"csi_{thr}"] = ("lead_time", csi_dict[thr])

    ds_out.lead_time.attrs["units"] = "hours"
    ds_out.attrs["description"] = "Half-hourly nowcast verification vs IMERG"

    ds_out.to_netcdf(out_nc)
    print(f"Wrote verification file: {out_nc}")


# -------------------------
# CLI
# -------------------------

if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Verify nowcast vs IMERG truth")
    ap.add_argument("--forecast-nc", required=True)
    ap.add_argument("--imerg-glob", required=True)
    ap.add_argument("--out-nc", required=True)
    ap.add_argument("--thresholds", nargs="+", type=float, default=[0.1, 1.0, 5.0])
    # coarsening options (default = no coarsening)
    ap.add_argument("--coarsen-lat", type=int, default=1,
                help="Latitudinal coarsening factor for IMERG truth (e.g., 10)")
    ap.add_argument("--coarsen-lon", type=int, default=1,
                help="Longitudinal coarsening factor for IMERG truth (e.g., 10)")

    args = ap.parse_args()

    verify(
        args.forecast_nc,
        args.imerg_glob,
        args.out_nc,
        thresholds=tuple(args.thresholds),
        coarsen_lat=args.coarsen_lat,
        coarsen_lon=args.coarsen_lon,
    )

