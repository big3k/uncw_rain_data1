# Run by cron every day, download 2-day's data: s to s+1   
#  notwithstanding e=s

s=$(date -d "-12 days" +%Y%m%d) 
e=$s 
#s=20260208 
#e=20250131

echo Downloading $s $e
/data1/tiany/MRMS/CONUS/download_PrecipRate.sh $s $e > /data1/tiany/MRMS/CONUS/log.${s}_$e.download_PrecipRate.txt 2>&1 
/data1/tiany/MRMS/CONUS/download_PrecipFlag.sh $s $e > /data1/tiany/MRMS/CONUS/log.${s}_$e.download_PrecipFlag.txt 2>&1 
/data1/tiany/MRMS/CONUS/download_RadarQualityIndex.sh $s $e > /data1/tiany/MRMS/CONUS/log.${s}_$e.download_RadarQualityIndex.txt 2>&1 
