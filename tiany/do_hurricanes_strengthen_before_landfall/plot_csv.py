#!/usr/bin/env python3

import argparse
import pandas as pd
import matplotlib.pyplot as plt


def main():
    parser = argparse.ArgumentParser(
        description="Plot CSV data as a multi-line plot."
    )

    parser.add_argument(
        "csv_file",
        help="Input CSV file"
    )

    parser.add_argument(
        "--yl",
        required=True,
        help="Y-axis label"
    )

    parser.add_argument(
        "-o",
        "--output",
        help="Output plot filename (PNG, PDF, JPG, etc.)"
    )

    parser.add_argument(
        "--dashed",
        nargs="*",
        default=[],
        help="Column names to draw with dashed lines"
    )

    parser.add_argument(
        "--colors",
        nargs="+",
        help="Colors for plotted data columns in order"
    )

    args = parser.parse_args()

    # Read CSV
    df = pd.read_csv(args.csv_file)

    if len(df.columns) < 2:
        raise ValueError(
            "CSV must contain at least one X column and one Y column."
        )

    # First column is X-axis
    x_col = df.columns[0]
    x = df[x_col]

    # Remaining columns are Y data series
    y_cols = list(df.columns[1:])

    # Validate color count
    if args.colors is not None:
        if len(args.colors) < len(y_cols):
            raise ValueError(
                f"{len(y_cols)} data columns found, "
                f"but only {len(args.colors)} colors supplied."
            )

    plt.figure(figsize=(10, 6))

    for i, col in enumerate(y_cols):

        linestyle = "--" if col in args.dashed else "-"

        plot_kwargs = {
            "label": col,
            "linestyle": linestyle,
            "linewidth": 2,
            "marker": "o"
        }

        if args.colors:
            plot_kwargs["color"] = args.colors[i]

        plt.plot(x, df[col], **plot_kwargs)

    plt.xlabel(x_col)
    plt.ylabel(args.yl)
    plt.legend(handlelength=4.0)
    plt.grid(True, alpha=0.3)
    plt.tight_layout()

    if args.output:
        plt.savefig(args.output, dpi=300)
        print(f"Plot saved to {args.output}")
    else:
        plt.show()


if __name__ == "__main__":
    main()

