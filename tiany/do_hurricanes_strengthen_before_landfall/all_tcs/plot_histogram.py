#!/usr/bin/env python3

import sys
import numpy as np
import matplotlib.pyplot as plt

if len(sys.argv) != 3:
    print(f"Usage: {sys.argv[0]} data.txt interval")
    sys.exit(1)

data = np.loadtxt(sys.argv[1], dtype=int)
interval = int(sys.argv[2])

# Create bins that cover the entire range
xmin = 0
xmax = int(np.max(data))

edges = np.arange(xmin, xmax + interval, interval)

# Histogram counts
counts, _ = np.histogram(data, bins=edges)

# Plot
plt.figure(figsize=(8,5))
plt.bar(edges[:-1], counts,
        width=interval,
        align='edge',
        edgecolor='black')

plt.xlabel("Value")
plt.ylabel("Count")
plt.title(f"Histogram (interval={interval})")

# Show every bin, including zero-count bins
step = max(1, len(edges) // 20)
plt.xticks(edges[::step], rotation=45)

plt.grid(True, alpha=0.3)
plt.tight_layout()

plt.savefig("histogram.png", dpi=200)
plt.show()
