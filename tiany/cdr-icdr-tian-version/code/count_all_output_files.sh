#! /bin/bash 

[ -f ../config ] || { echo "config file does not exist. exit."; exit; }

. ../config   # set up user-defined variables. 

# header
echo "date, satellite, sensor, fcdr-pre, tcdr-pre, tcdr" 

 iday=$start_day
 while [ $iday -le $end_day ]; do
  yyyymmdd=$iday

  for sat in $sats; do
   for sensor in amsua mhs; do 
    out_dir=$top_dir/output/fcdr-pre/$sensor/$sat/$yyyymmdd/
    fc_nfiles=`ls $out_dir/*.nc 2> /dev/null |wc -l`
    out_dir=$top_dir/output/tcdr-pre/$sensor/$sat/$yyyymmdd/
    tc_nfiles=`ls $out_dir/*.nc 2> /dev/null |wc -l`
    out_dir=$top_dir/output/tcdr/$sensor/$sat/$yyyymmdd/
    final_tc_nfiles=`ls $out_dir/*.nc 2> /dev/null |wc -l`
    echo "$yyyymmdd,$sat,$sensor,$fc_nfiles, $tc_nfiles, $final_tc_nfiles"
   done
  done
  
  iday=`date -u -d "$iday +1 day" +%Y%m%d`

 done


