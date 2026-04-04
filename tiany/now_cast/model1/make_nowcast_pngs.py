#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Generate half-hourly PNG frames from a precipitation forecast NetCDF file.

Features:
- Half-hourly lead-time handling
- Logarithmic color scale
- Small negative values clipped to 0 (not blank)
- Zero rainfall renders correctly (no holes)
- Suitable for GIF animation

Input:
  NetCDF with variable:
    precipitation_forecast(time, lat, lon)  [mm/hr]

Output:
  One PNG per lead time:
    frame_000.png, frame_001.png, ...

Author: Nowcasting Visualization Pipeline
"""

import argparse
import os
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm


def main(ncfile, outdir, vmax):
    os.makedirs(outdir, exist_ok=True)

    # Load forecast NetCDF
    ds = xr.open_dataset(ncfile)
    da = ds["precipitation_forecast"]

    times = da.time.values
    lat = da.lat.values
    lon = da.lon.values

    # Fixed color normalization (log scale)
    vmin = 0.1
    norm = LogNorm(vmin=vmin, vmax=vmax)

    for i, t in enumerate(times):
        # Extract frame
        frame = da.isel(time=i).values

        # --- IMPORTANT FIX ---
        # Treat small negative numerical noise as zero rainfall
        frame = np.where(frame < 0, 0.0, frame)

        # Add tiny offset so zeros plot correctly under LogNorm
        frame_plot = frame + 1e-6

        # Time string for title
        tstr = np.datetime_as_string(t, unit="m")

        fig, ax = plt.subplots(figsize=(10, 5))

        im = ax.pcolormesh(
            lon,
            lat,
            frame_plot,
            cmap="turbo",
            norm=norm,
            shading="auto"
        )

        ax.set_title(f"IMERG Half-Hourly Nowcast\nValid: {tstr} UTC")
        ax.set_xlabel("Longitude")
        ax.set_ylabel("Latitude")
        ax.set_xlim(lon.min(), lon.max())
        ax.set_ylim(lat.min(), lat.max())

        # Colorbar
        cb = fig.colorbar(im, ax=ax, pad=0.02)
        cb.set_label("Precipitation (mm/hr)")

        # Save frame
        fname = f"frame_{i:03d}.png"
        fig.savefig(
            os.path.join(outdir, fname),
            dpi=150,
            bbox_inches="tight"
        )
        plt.close(fig)

        print(f"Wrote {fname}")

    print(f"All PNG frames written to: {outdir}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser(
        description="Generate PNG frames from half-hourly nowcast NetCDF"
    )
    ap.add_argument(
        "--nc",
        required=True,
        help="Forecast NetCDF file (e.g., forecast_halfhour.nc)"
    )
    ap.add_argument(
        "--outdir",
        required=True,
        help="Output directory for PNG frames"
    )
    ap.add_argument(
        "--vmax",
        type=float,
        default=30.0,
        help="Maximum value for color scale (mm/hr)"
    )

    args = ap.parse_args()
    main(args.nc, args.outdir, args.vmax)

