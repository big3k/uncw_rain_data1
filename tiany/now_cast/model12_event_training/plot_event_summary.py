#!/usr/bin/env python3

from pathlib import Path
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt

# --------------------------------------------------
# Configuration
# --------------------------------------------------

ROOT = Path("2023_events_nowcast")
OUTDIR = Path("event_summary_plots")
OUTDIR.mkdir(exist_ok=True)

# --------------------------------------------------
# Find verification files
# --------------------------------------------------

files = sorted(ROOT.glob("*/verification_0.5_halfhour.nc"))

if len(files) == 0:
    raise RuntimeError(
        f"No verification files found under {ROOT}"
    )

print(f"Found {len(files)} verification files")

# --------------------------------------------------
# Load datasets
# --------------------------------------------------

events = []
datasets = []

for f in files:
    events.append(f.parent.name)
    datasets.append(xr.open_dataset(f))

events = np.array(events)


lead_times = datasets[0]["lead_time"].values

# Convert timedelta64 to hours
if np.issubdtype(lead_times.dtype, np.timedelta64):
    lead_times = lead_times / np.timedelta64(1, "h")

# Force float, and make starting time as 0.5 hr. 
lead_times = np.asarray(lead_times + 0.5, dtype=float)

nlead = len(lead_times)

print("Events:")
print(", ".join(events))

print("Lead times (hours):")
print(lead_times)

# --------------------------------------------------
# Metrics to plot
# --------------------------------------------------

metrics = [
    "rmse",
    "mae",
    "bias",
    "corr",
    "pod_0.1",
    "far_0.1",
    "csi_0.1",
    "pod_1.0",
    "far_1.0",
    "csi_1.0",
    "pod_5.0",
    "far_5.0",
    "csi_5.0",
]

# --------------------------------------------------
# Color scheme
# --------------------------------------------------

def metric_color(metric):

    if metric.startswith("corr"):
        return "forestgreen"

    if metric.startswith("pod"):
        return "royalblue"

    if metric.startswith("csi"):
        return "darkorange"

    if metric.startswith("far"):
        return "firebrick"

    if metric in ["rmse", "mae"]:
        return "steelblue"

    if metric == "bias":
        return "purple"

    return "gray"


# --------------------------------------------------
# Plot one figure per metric
# --------------------------------------------------

for metric in metrics:

    if metric not in datasets[0].data_vars: 
        print(f"Skipping missing metric: {metric}")
        continue

    print(f"Plotting {metric}")

    fig, axes = plt.subplots(
        nlead,
        1,
        figsize=(16, max(10, 2.5 * nlead)),
        sharex=True,
        dpi=150
    )

    if nlead == 1:
        axes = [axes]

    color = metric_color(metric)

    for ilead, lead in enumerate(lead_times):

        vals = []

        for ds in datasets:
            vals.append(
                float(ds[metric].isel(lead_time=ilead))
            )

        vals = np.array(vals)

        ax = axes[ilead]

        ax.bar(
            np.arange(len(events)),
            vals,
            color=color
        )

        ax.grid(
            axis="y",
            linestyle="--",
            alpha=0.3
        )

        ax.set_ylabel(metric)

        ax.set_title(
            f"{metric.upper()}  @  +{lead:.1f} h",
            fontsize=10
        )

        # Fixed scale for skill scores
        if (
            metric.startswith("corr")
            or metric.startswith("pod")
            or metric.startswith("far")
            or metric.startswith("csi")
        ):
            ax.set_ylim(0, 1)

        ax.set_xlim(-0.5, len(events) - 0.5)

    # x-axis labels only on bottom panel
    axes[-1].set_xticks(np.arange(len(events)))
    axes[-1].set_xticklabels(
        events,
        rotation=90,
        fontsize=8
    )

    axes[-1].set_xlabel("2023 Tropical Cyclone Event")

    fig.suptitle(
        f"{metric.upper()} Across 2023 Events",
        fontsize=16,
        y=0.995
    )

    fig.tight_layout()

    outfile = OUTDIR / f"{metric}.png"

    fig.savefig(
        outfile,
        bbox_inches="tight"
    )

    plt.close(fig)

    print(f"Wrote {outfile}")

print()
print(f"Finished. Figures written to: {OUTDIR}")

