#!/usr/bin/env python3

import sys
import glob
from pathlib import Path

import numpy as np
import scipy.io
import matplotlib.pyplot as plt
import matplotlib.cm as cm

import cartopy.crs as ccrs
import cartopy.feature as cfeature


def load_track(matfile):
    """Load lat/lon track from a MAT file."""
    try:
        mat = scipy.io.loadmat(
            matfile,
            squeeze_me=True,
            struct_as_record=False
        )

        if "hur" not in mat:
            return None

        hur = mat["hur"]

        lat = np.asarray(hur.lat).astype(float)
        lon = np.asarray(hur.lon).astype(float)

        if len(lat) == 0 or len(lon) == 0:
            return None

        return lat, lon

    except Exception as e:
        print(f"Failed: {matfile} ({e})")
        return None


def main():

    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} start_year end_year")
        sys.exit(1)

    start_year = int(sys.argv[1])
    end_year = int(sys.argv[2])

    years = range(start_year, end_year + 1)

    fig = plt.figure(figsize=(18, 9))
    ax = plt.axes(projection=ccrs.PlateCarree())

    # Map background
    ax.add_feature(cfeature.LAND, facecolor="lightgray")
    ax.add_feature(cfeature.OCEAN, facecolor="white")
    ax.add_feature(cfeature.COASTLINE, linewidth=0.5)
    ax.add_feature(cfeature.BORDERS, linewidth=0.3)

    ax.gridlines(draw_labels=True, linewidth=0.25)

    ax.set_global()

    colors = cm.jet(np.linspace(0, 1, len(years)))

    total_storms = 0

    for color, year in zip(colors, years):

        year_dir = Path(str(year))

        if not year_dir.exists():
            print(f"Missing directory: {year}")
            continue

        files = sorted(glob.glob(f"{year}/*.mat"))

        print(f"{year}: {len(files)} storms")

        for f in files:

            track = load_track(f)

            if track is None:
                continue

            lat, lon = track

            ax.plot(
                lon,
                lat,
                color=color,
                linewidth=0.7,
                alpha=0.6,
                transform=ccrs.PlateCarree()
            )

            total_storms += 1

    plt.title(
        f"Global Tropical Cyclone Tracks ({start_year}-{end_year})\n"
        f"{total_storms} storms",
        fontsize=16
    )

    outfile = f"storm_tracks_{start_year}_{end_year}.png"

    plt.savefig(
        outfile,
        dpi=300,
        bbox_inches="tight"
    )

    print(f"\nSaved: {outfile}")
    print(f"Total storms plotted: {total_storms}")


if __name__ == "__main__":
    main()
