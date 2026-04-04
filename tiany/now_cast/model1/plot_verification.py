#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Plot verification metrics from a half-hourly nowcast verification NetCDF file.

Input:
  verification_halfhour.nc

Outputs:
  <prefix>_skill.png  (RMSE, MAE vs lead time)
  <prefix>_corr.png   (Correlation vs lead time)
  <prefix>_csi.png    (CSI vs lead time for all thresholds)

Author: Nowcasting Verification Pipeline
"""

import numpy as np
import argparse
import xarray as xr
import matplotlib.pyplot as plt


def plot_skill(ds, lead_hours, prefix):
    plt.figure(figsize=(7, 5))
    plt.plot(lead_hours, ds.rmse, marker="o", label="RMSE")
    plt.plot(lead_hours, ds.mae, marker="s", label="MAE")

    plt.xlabel("Lead time (hours)")
    plt.ylabel("mm / hr")
    plt.title("Nowcast Error vs Lead Time")
    plt.grid(True)
    plt.legend()

    out = f"{prefix}_skill.png"
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    plt.close()

    print(f"Wrote {out}")


def plot_corr(ds, lead_hours, prefix):
    plt.figure(figsize=(7, 5))
    plt.plot(lead_hours, ds.corr, marker="o")

    plt.xlabel("Lead time (hours)")
    plt.ylabel("Correlation")
    plt.title("Spatial Correlation vs Lead Time")
    plt.ylim(0, 1)
    plt.grid(True)

    out = f"{prefix}_corr.png"
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    plt.close()

    print(f"Wrote {out}")


def plot_csi(ds, lead_hours, prefix):
    plt.figure(figsize=(7, 5))

    for var in ds.variables:
        if var.startswith("csi_"):
            thr = var.split("_", 1)[1]
            plt.plot(lead_hours, ds[var], marker="o",
                     label=f"≥ {thr} mm/hr")

    plt.xlabel("Lead time (hours)")
    plt.ylabel("CSI")
    plt.title("Event Skill (CSI) vs Lead Time")
    plt.grid(True)
    plt.legend()

    out = f"{prefix}_csi.png"
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    plt.close()

    print(f"Wrote {out}")


def main():
    ap = argparse.ArgumentParser(
        description="Plot nowcast verification metrics from NetCDF"
    )
    ap.add_argument(
        "verification_nc",
        help="Verification NetCDF file (e.g., verification_halfhour.nc)"
    )
    ap.add_argument(
        "--prefix",
        default=None,
        help="Output file prefix (default: NetCDF filename without .nc)"
    )
    args = ap.parse_args()

    ds = xr.open_dataset(args.verification_nc)
    
    # convert lead_time from timedelta → hours (float)
    lead_hours = ds.lead_time.astype("timedelta64[ns]").astype(float) / 3.6e12

    print(lead_hours)

    prefix = args.prefix
    if prefix is None:
        prefix = args.verification_nc.replace(".nc", "")

    print(f"Plotting verification metrics from {args.verification_nc}")
    print(f"Output prefix: {prefix}")

    plot_skill(ds, lead_hours, prefix)
    plot_corr(ds, lead_hours, prefix)
    plot_csi(ds, lead_hours, prefix)


if __name__ == "__main__":
    main()
