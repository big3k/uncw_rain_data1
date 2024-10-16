#! /bin/bash 

#Input dir format example: 
# /data1/youy/cdr-atms-combine/gatmo/2021/20210103/
#Output dir format example
# /tmp/2021/20210103/

#data location above yyyy/yyyymmdd/
daily_gatmo_dir="/data1/youy/cdr-atms-combine/gatmo/" 
daily_tatms_dir="/data1/youy/cdr-atms-combine/tatms/" 
daily_output_dir="/tmp/"   # make sure user has write permission 

# ===  User Configuration === 
sday=20120302  # start day
eday=20120303  # end day 

# ===  End User Configuration === 

iday=$sday
while [ $iday -le $eday ]; do 

echo  Processing $iday 
year=$(date -u -d "$iday" +%Y)

mkdir -p $daily_output_dir/$year/$iday
./merge_npp_atms_daily $daily_gatmo_dir/$iday $daily_tatms_dir/$iday $daily_output_dir/$year/$iday 

iday=$(date -u -d "$iday + 1 day" +%Y%m%d) 

done 

