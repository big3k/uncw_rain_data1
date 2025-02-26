#somehow crontab did not run. so do it here
s=20240701
e=20250131

/data1/tiany/MRMS/CONUS/download_PrecipRate.sh $s $e > /data1/tiany/MRMS/CONUS/log.$s_$e.download_PrecipRate.txt 2>&1 
/data1/tiany/MRMS/CONUS/download_PrecipFlag.sh $s $e > /data1/tiany/MRMS/CONUS/log.$s_$e.download_PrecipFlag.txt 2>&1 
/data1/tiany/MRMS/CONUS/download_RadarQualityIndex.sh > /data1/tiany/MRMS/CONUS/log.`date +%Y%m%d`.download_RadarQualityIndex.txt 2>&1 
