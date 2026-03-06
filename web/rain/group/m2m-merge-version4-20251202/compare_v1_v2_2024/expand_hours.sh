#!/usr/bin/env bash
# Expand CSV rows into one row per hour.
# Input CSV columns: event,day,problematic hours
# Output columns: event,day,hour
#
# - Skips rows where the hours column is empty.
# - Accepts an input file path or reads from STDIN.
# - Handles trailing colons like "16:17:" and trims whitespace.
# - Filters out invalid/non-numeric hours and keeps only 0–23.

set -euo pipefail

# Usage/help
if [[ "${1-}" == "-h" || "${1-}" == "--help" ]]; then
  cat <<'USAGE'
Usage:
  ./expand_hours.sh input.csv > expanded.csv
  cat input.csv | ./expand_hours.sh > expanded.csv

Description:
  Reads a CSV with columns: event,day,problematic hours
  and outputs an expanded CSV with columns: event,day,hour.
  Rows with empty hours are ignored.
USAGE
  exit 0
fi

# Choose input: file or stdin
if [[ $# -ge 1 && -n "${1-}" && "$1" != "-" ]]; then
  INFILE="$1"
else
  INFILE="-"
fi

awk -F',' '
BEGIN {
  OFS = ",";
  print "event","day","hour";  # output header
}
NR==1 {
  # Skip input header (assumes first line is header)
  next;
}
{
  # Extract fields and trim spaces
  event = $1;
  day   = $2;
  hours = $3;

  # Trim leading/trailing spaces
  gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", event);
  gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", day);
  gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", hours);

  # Skip if hours field is empty
  if (hours == "" || hours ~ /^[ \t\r\n]*$/) next;

  # Split by colon
  n = split(hours, a, ":");

  # Iterate tokens; ignore empty tokens (from trailing colon) and invalids
  for (i = 1; i <= n; i++) {
    token = a[i];
    gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", token);
    if (token == "") continue;

    # Validate numeric 0–23
    if (token ~ /^[0-9]+$/) {
      h = token + 0;
      if (h >= 0 && h <= 23) {
        printf "%s,%s,%2.2d\n", event, day, h;
      }
    }
  }
}
' "$INFILE"
``

