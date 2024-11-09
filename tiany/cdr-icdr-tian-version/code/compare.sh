
for newfile in /data1/tiany/cdr-icdr-tian-version/output/fcdr-pre/amsua/noaa-18/20240801/*.nc; do 
  fname=`basename $newfile`
  oldfile=/data1/tiany/tmp/fcdr-pre/amsua/noaa-18/2024/$fname
  echo =============================================================
  echo $oldfile 
  echo $newfile 
  ncdump -v fcdr_brightness_temperature_23 $oldfile > old_data 
  ncdump -v fcdr_brightness_temperature_23 $newfile > new_data 
  diff old_data new_data 

done
