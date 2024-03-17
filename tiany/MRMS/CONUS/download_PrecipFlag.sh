#! /usr/bin/bash
# 3/10/2024: 
#  Download 2-min MRMS from 
# https://noaa-mrms-pds.s3.amazonaws.com/index.html#CONUS/PrecipRate_00.00/20201017/

# Sample URL 
# https://noaa-mrms-pds.s3.amazonaws.com/CONUS/PrecipFlag_00.00/20201014/MRMS_PrecipFlag_00.00_20201014-212400.grib2.gz

base_dir=/data1/tiany/MRMS/CONUS
var_name="PrecipFlag_00.00"
#--------------------------------------
start_day=20201014 
end_day=20240229
#--------------------------------------

ssec=`date -d "$start_day" +%s`
esec=`date -d "$end_day" +%s`

nday=`awk "BEGIN{ print ($esec-$ssec)/(24*60*60) }"`


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

