#! /bin/bash 

[ -f ./config ] || { echo "config file does not exist. exit."; exit; }

. ./config   # set up user-defined variables. 

mkdir -p $top_dir/logs
run_date=`date +%FT%H-%M-%S`

# Pre-process GFS data 
./code/extract_gfs.sh  > $top_dir/logs/fcdr_amsua_${run_date}.log 2>&1

# FCDR and TCDR. 
# For each satellite, amsua has to be done first as amsub/mhs depends on it 
for sat in $sats; do 
 ./code/proc_fcdr.sh amsua $sat  > $top_dir/logs/fcdr_amsua_${run_date}.log 2>&1
 ./code/proc_tcdr.sh amsua $sat  > $top_dir/logs/tcdr_amsua_${run_date}.log 2>&1
 ./code/proc_fcdr.sh mhs $sat  > $top_dir/logs/fcdr_mhs_${run_date}.log 2>&1
 ./code/proc_tcdr.sh mhs $sat  > $top_dir/logs/ccdr_mhs_${run_date}.log 2>&1
done


