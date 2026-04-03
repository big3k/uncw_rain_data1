#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Generate half-hourly PNGs from a precipitation forecast NetCDF.

Input:
- NetCDF with variable: precipitation_forecast(time, lat, lon) [mm/hr]

Output:
- One PNG per lead time with consistent color scaling
"""

import argparse
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt
import os
from matplotlib.colors import LogNorm


def main(ncfile, outdir, vmax):
    os.makedirs(outdir, exist_ok=True)

    ds = xr.open_dataset(ncfile)
    da = ds["precipitation_forecast"]

    times = da.time.values
    lat = da.lat.values
    lon = da.lon.values

    # Fixed log scale for precipitation
    vmin = 0.1
    norm = LogNorm(vmin=vmin, vmax=vmax)

    for i, t in enumerate(times):
        frame = da.sel(time=t).values

        # Convert time to readable string
        tstr = np.datetime_as_string(t, unit="m")

        fig, ax = plt.subplots(figsize=(10, 5))
        im = ax.pcolormesh(
            lon, lat, frame,
            cmap="turbo",
            norm=norm,
            shading="auto"
        )

        ax.set_title(f"IMERG Half-Hourly Nowcast\nValid: {tstr} UTC")
        ax.set_xlabel("Longitude")
        ax.set_ylabel("Latitude")
        ax.set_xlim(lon.min(), lon.max())
        ax.set_ylim(lat.min(), lat.max())
        ax.grid(False)

        cb = fig.colorbar(im, ax=ax, pad=0.02)
        cb.set_label("Precipitation (mm/hr)")

        fname = f"frame_{i:03d}.png"
        fig.savefig(os.path.join(outdir, fname), dpi=150, bbox_inches="tight")
        plt.close(fig)

        print(f"Wrote {fname}")

    print(f"All PNGs saved in {outdir}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--nc", required=True, help="Forecast NetCDF file")
    ap.add_argument("--outdir", required=True, help="Output directory for PNGs")
    ap.add_argument("--vmax", type=float, default=30.0,
                    help="Max colorbar value (mm/hr)")
    args = ap.parse_args()

    main(args.nc, args.outdir, args.vmax)
