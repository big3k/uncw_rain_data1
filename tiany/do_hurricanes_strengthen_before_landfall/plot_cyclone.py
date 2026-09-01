#!/usr/bin/env python3

import sys
import scipy.io
import numpy as np
import matplotlib.pyplot as plt

if len(sys.argv) != 2:
    print(f"Usage: {sys.argv[0]} storm.mat")
    sys.exit(1)

matfile = sys.argv[1]

# Load MATLAB file
mat = scipy.io.loadmat(matfile, squeeze_me=True, struct_as_record=False)

if "hur" not in mat:
    print("Error: variable 'hur' not found in MAT file")
    sys.exit(1)

hur = mat["hur"]

lat = np.asarray(hur.lat)
lon = np.asarray(hur.lon)

storm_name = getattr(hur, "name", "Unknown")

# Create plot
fig, ax = plt.subplots(figsize=(8, 6))

ax.plot(lon, lat, "-o", lw=2, ms=4, color="blue", label="Track")

# start point
ax.plot(lon[0], lat[0], "go", ms=10, label="Start")

# end point
ax.plot(lon[-1], lat[-1], "ro", ms=10, label="End")

ax.set_xlabel("Longitude")
ax.set_ylabel("Latitude")
ax.set_title(f"Cyclone Track: {storm_name}")

ax.grid(True)
ax.legend()

# Add a little padding
ax.set_xlim(lon.min() - 2, lon.max() + 2)
ax.set_ylim(lat.min() - 2, lat.max() + 2)

plt.tight_layout()

outfile = "storm_track.png"
plt.savefig(outfile, dpi=200)
print(f"Saved: {outfile}")

plt.show()
