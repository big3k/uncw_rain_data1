#! /usr/bin/bash

# Usage: 
# $0 [yyyymmdd yyyymmdd]

# 8/1/2024:
#  -- let user enter start_date and end_date (yyyymmdd). If not given, use the current month.

# 4/7/2024: 
# - changed time frame to the past month, so to run in cron job
# 3/10/2024: 
#  Download 2-min MRMS from 
# https://noaa-mrms-pds.s3.amazonaws.com/index.html#CONUS/PrecipRate_00.00/20201017/

# Sample URL 
# https://noaa-mrms-pds.s3.amazonaws.com/CONUS/PrecipRate_00.00/20201014/MRMS_PrecipRate_00.00_20201014-212200.grib2.gz

base_dir=/data1/tiany/MRMS/CONUS
var_name="PrecipRate_00.00"
#--------------------------------------
if [ $# -eq 2 ]; then
 start_day=$1
 end_day=$2
else
 start_day=$(date -d "last month" +%Y%m01)
 end_day=$(date -d "`date +%Y%m01` -1 day" +%Y%m%d)
fi

echo $start_day
echo $end_day
#--------------------------------------

ssec=`date -d "$start_day" +%s`
esec=`date -d "$end_day 23:59:59" +%s`

nday=`awk "BEGIN{ print int(($esec-$ssec)/(24*60*60)+0.5) }"`

echo $nday

# download every day
for day in `seq 0 $nday`; do 
 local_date=`date -d "$start_day + $day day" +%Y%m%d` 
 mkdir -p $base_dir/$var_name/$local_date
  
 echo Downloading $var_name $local_date 
 # data file every 2 min 
 for min in `seq 0 2 1438`; do 
   ftime=`date -d "$local_date + $min min" +%Y%m%d-%H%M00`  
   fname=MRMS_${var_name}_${ftime}.grib2.gz
   wget -O $base_dir/$var_name/$local_date/$fname https://noaa-mrms-pds.s3.amazonaws.com/CONUS/$var_name/$local_date/$fname
 done # every 2m in

done  # every day 

