#! /usr/bin/bash

# Usage:
# $0 [start_yyyymmdd end_yyyymmdd]
# User enter start_date and end_date (yyyymmdd). 

# What it does: 
#
# Lists what *_000.grb2 and *_003.grb2 files are missing.    

# ------------------ config params
unpack_dir=/data1/tiany/GFS_auto_download/GFS_00-03 # dir to contain unpacked/weeded files 
# ------------------ end config params

if [ $# -eq 2 ]; then
 start_day0=$1
 end_day0=$2
else
  echo "Usage: $0 start_yyyymmdd end_yyyymmdd" 
  exit -1
fi

start_day=$start_day0

while [ $start_day -le $end_day0 ]; do 
   # look for files for each day
   for fx in 0000 0600 1200 1800; do 
     for fh in 000 003; do 
       oldf=$unpack_dir/$start_day/gfs_3_${start_day}_${fx}_f${fh}.grib
       newf=$unpack_dir/$start_day/gfs_3_${start_day}_${fx}_${fh}.grb2  # new file format 
       if [ ! -e $oldf -a ! -e $newf ]; then 
          echo "$oldf or gfs_3_${start_day}_${fx}_${fh}.grb2 is missing" 
       fi
     done #fh
   done #fx
 # move ahead 
   start_day=$(date -d "$start_day +1 day" +%Y%m%d) 

done 


