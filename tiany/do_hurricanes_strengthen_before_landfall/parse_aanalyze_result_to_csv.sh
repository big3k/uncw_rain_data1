#!/bin/bash
# parse out the mean rainrate from the "analyze_time_evolution_imerg_*.m" runs. 
# Usage: 
# parse_aanalyze_result_to_csv.sh <input_file> <output_csv> <col_name_of_rainrate> 
#  Example: 
# 
# ./parse_aanalyze_result_to_csv.sh result.analyze_time_evolution_imerg_hq.txt meanrr_imerg_hq.csv "Land_HQ"

#infile="result.analyze_time_evolution_imerg_hq.txt"
#outfile="rain_rate.csv"
infile=$1
outfile=$2 
col_name="$3"

echo "Hours,$3" > "$outfile"

awk '
/^-+[[:space:]]*$/ {in_table=1; next}
in_table && $2=="h" {
    print $1 "," $3
}
' "$infile" >> "$outfile"

