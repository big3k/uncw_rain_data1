#! /usr/bin/bash
# 3/10/2024: 
#  Download 2-min MRMS from 
# https://noaa-mrms-pds.s3.amazonaws.com/index.html#CONUS/PrecipRate_00.00/20201017/

# Sample URL 
#https://noaa-mrms-pds.s3.amazonaws.com/CONUS/RadarQualityIndex_00.00/20201014/MRMS_RadarQualityIndex_00.00_20201014-214000.grib2.gz

base_dir=/data1/tiany/MRMS/CONUS
var_name="RadarQualityIndex_00.00"
#--------------------------------------
#start_day=20201014
#end_day=20240229

start_day=$(date -d "last month" +%Y%m01)
end_day=$(date -d "`date +%Y%m01` -1 day" +%Y%m%d)

echo $start_day
echo $end_day
#--------------------------------------

ssec=`date -d "$start_day" +%s`
esec=`date -d "$end_day" +%s`

echo $esec $ssec
nday=`awk "BEGIN{ print int(($esec-$ssec)/(24*60*60)+0.5) }"`

echo $nday


# download every day
for day in `seq 0 $nday`; do 
 local_date=`date -d "$start_day + $day day" +%Y%m%d` 
 # data file every 2 min 
 for min in `seq 0 2 1438`; do 
   ftime=`date -d "$local_date + $min min" +%Y%m%d-%H%M00`  
    echo $ftime
 done # every 2m in

done  # every day 

