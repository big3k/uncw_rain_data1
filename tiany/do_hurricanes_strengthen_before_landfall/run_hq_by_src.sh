
mkdir HQ_SRC

for src in ALL SSMI SSMIS GMI MHS AMSR2; do 
  matlab -batch "analyze_time_evolution_imerg_hq_by_src('$src')" > HQ_SRC/result_$src.txt
  ./parse_aanalyze_result_to_csv.sh HQ_SRC/result_$src.txt HQ_SRC/$src.csv "Land_$src"
done




