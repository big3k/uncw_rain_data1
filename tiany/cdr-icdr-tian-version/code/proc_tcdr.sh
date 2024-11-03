#! /bin/bash
# Function: process ccdr for given satellite and sensor 
# Usage:  $0  sensor_name<amsua|mhs> sat_name<metop-a|noaa-18|noaa-19>
# Example: $0 amsua noaa-18

[ $# -eq 2 ] || { echo "Usage: $0 sensor_name sat_name"; exit -1; }

sensor=$1
sat=$2

. ../config

[ $start_day -le $end_day ] || { echo Start day is later than end day!; exit -1; } 

# go to the right code base
if [ "X$sensor" == "Xamsua" ]; then 
  cd $top_dir/code/amsua-tcdr-pre/ama2nc
elif [ "X$sensor" == "Xmhs" ]; then 
  cd $top_dir/code/amsub-tcdr-pre/amb2nc
else 
 echo "wrong sensor name: $1. exit." 
 exit -1
fi

# run fcdr, day by day

iday=$start_day
while [ $iday -le $end_day ]; do 

  yyyymmdd=$iday
  out_dir=$top_dir/output/tcdr-pre/$sensor/$sat/$yyyymmdd/
  mkdir -p $out_dir

  echo TCDR Processing $yyyymmdd
  # FCDR file list for ama2nc 
   input_dir=$top_dir/output/fcdr-pre/$sensor/$sat/$yyyymmdd/
  # GFS input file dir 
   gfs_input_dir=$top_dir/input/proc_avn/$yyyymmdd/
   # temp solution for overcoming the long directory name issue: 
   # runama will crash if input_dir and output_dir are too long
   ln -snf $input_dir tmp_input_dir
   ln -snf $out_dir tmp_output_dir
   ln -snf $gfs_input_dir tmp_gfs_dir
   #echo runama: $input_dir $out_dir
   if [ "X$sensor" == "Xamsua" ]; then
     ls $input_dir > amafile
     time ./runama tmp_input_dir/ tmp_gfs_dir/ tmp_output_dir/ > ama2nc.log 2> ama2nc.error
   elif [ "X$sensor" == "Xmhs" ]; then
     ls $input_dir | grep MHSX | grep -v ATT1 > file_list
     wc -l file_list > fileA.length # need this!
     time ./run1b2nc tmp_input_dir/ tmp_output_dir/ > mhs2nc.log 2> mhs2nc.error 
   else
     echo "wrong sensor name: $1. exit."
    exit -1
   fi
   #rm tmp_input_dir tmp_output_dir

   iday=`date -u -d "$iday +1 day" +%Y%m%d`
done

exit

