#!/usr/bin/env bash
set -euo pipefail

NC=$1                         # forecast_*.nc
BASENAME=$(basename "$NC" .nc)

PNGDIR="png_${BASENAME}"
GIFOUT="${BASENAME}.gif"

python make_nowcast_pngs.py \
  --nc "$NC" \
  --outdir "$PNGDIR" \
  --vmax 30

python make_gif.py \
  --frames "$PNGDIR" \
  --out "$GIFOUT" \
  --fps 4

echo "Done: $GIFOUT"
