#!/usr/bin/env python3

import pandas as pd
import matplotlib.pyplot as plt

# Read CSV
df = pd.read_csv(
    "output.step2.csv",
    header=None,
    names=["tc_name", "yyyyyod", "hour", "mean_rr", "filename"]
)

df["yyyyyod"] = df["yyyyyod"].astype(int)
df["hour"] = df["hour"].astype(int)
df["mean_rr"] = df["mean_rr"].astype(float)

plt.figure(figsize=(12, 8))

all_tc = [] # <-- initialize here
n_tc = 0

for tc_name, g in df.groupby("tc_name"):

    g = g.sort_values(["yyyyyod", "hour"])

    x = range(0, 3 * len(g), 3)
    y = g["mean_rr"].values

    plt.plot(
        x,
        y,
        linewidth=0.7,
        alpha=0.3
    )

    tmp = pd.DataFrame({
              "x": x,
              "mean_rr": y
    })
    all_tc.append(tmp)

    n_tc += 1

# Combine all TCs
all_tc = pd.concat(all_tc, ignore_index=True)

stats = (
    all_tc.groupby("x")["mean_rr"]
    .agg(["mean", "std"])
    .reset_index()
)

plt.fill_between(
    stats["x"],
    stats["mean"] - stats["std"],
    stats["mean"] + stats["std"],
    color="black",
    alpha=0.2
)

plt.plot(
    stats["x"],
    stats["mean"],
    color="black",
    linewidth=4,
    label="Mean of all TCs"
)

plt.xlabel("Hours since first record")
plt.ylabel("Mean rain rate (mm h$^{-1}$)")
plt.title(f"Mean Rain Rate Evolution for {n_tc} Tropical Cyclones")
plt.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig("all_tc_mean_rr.png", dpi=300)

print(f"Plotted {n_tc} tropical cyclones")
print("Saved: all_tc_mean_rr.png") 



