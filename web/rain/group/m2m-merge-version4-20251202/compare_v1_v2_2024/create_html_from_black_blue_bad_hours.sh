
#sample data
#YAGI,20240905,23
#YAGI,20240906,03
#

# images localtion
# V1:  step12-hourly-image/2024/ANGGREK/20240130-1600.png
# V2:  step12-hourly-image-v2/2024/ANGGREK/20240130-1600.png

cat black_blue_bad_hours.csv |while read hourly; do 
  event=`echo $hourly |cut -d, -f1`
  day=`echo $hourly |cut -d, -f2`
  year=`echo $day |cut -c1-4`
  hr=`echo $hourly |cut -d, -f3`
  echo "<img src='step12-hourly-image/$year/$event/${day}-${hr}00.png'><img src='step12-hourly-image-v2/$year/$event/${day}-${hr}00.png'>"  
done

