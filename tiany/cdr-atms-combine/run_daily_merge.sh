#! /bin/bash 

# ===  User Configuration === 
sday=20120302  # start day
eday=20120303  # end day 

daily_gatmo_dir="/data1/youy/cdr-atms-combine/gatmo/" 
daily_tatms_dir="/data1/youy/cdr-atms-combine/tatms/" 
daily_output_dir="/tmp/"   # make sure user has write permission 
# ===  End User Configuration === 

iday=$sday
while [ $iday -le $eday ]; do 

echo  Processing $iday 

mkdir -p $daily_output_dir/$iday
./merge_npp_atms_daily $daily_gatmo_dir/$iday $daily_tatms_dir/$iday $daily_output_dir/$iday 

iday=$(date -u -d "$iday + 1 day" +%Y%m%d) 

done 

