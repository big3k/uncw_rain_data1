# 1/19/2026: 
# Workflow description: 
# 1. On https://lswg.uncw.edu/group/m2m-merge-version4-20251202/review_v1_v2.html user reviews results 
#    with the "Good" and "Bad" buttons. 
# 2. Button clicks were recorded in /var/log/httpd/ssl_lswg_access.log
# 3. The relevant entries from #2 were extractred into v2_still_bad_in_httpd.log
# 4. From v2_still_bad_in_httpd.log another html page is created to further classify/diagnose issues. 

# Get the list of still bad v2 events: 
# sample output:

# step12-hourly-image-v2/2024/MILTON/20241009-1800.png
# step12-hourly-image-v2/2024/NEVILLE/20240312-0900.png
# step12-hourly-image-v2/2024/NEVILLE/20240312-1100.png
# step12-hourly-image-v2/2024/NEVILLE/20240318-0600.png

# Need to find from: 
# step12-hourly-image-v2/2024/ANGGREK/F18-20240116-1000.png
# from: 
# step12-hourly-image-v2/2024/ANGGREK/20240116-1000.png

cat v2_still_bad_in_httpd.log |awk -F'&' '{print $3}' |sed -e 's#%2F#/#g'  -e 's/right=//'g \
 |while read hourly; do 
  event=`echo $hourly |cut -d/ -f3`
  day=`echo $hourly |cut -d/ -f4 |cut -d- -f1`
  year=`echo $hourly |cut -d/ -f2` 
  hr=`echo $hourly |cut -d/ -f4 |cut -d- -f2 |sed -e 's/00.png//'`  # 10
  path=`echo $hourly |cut -d/ -f1`
  #echo $year/$event/$day-$hr 
  file=`ls ../$path/$year/$event/[A-Z]*$day-$hr*.png`
  # e.g.: ../step12-hourly-image-v2/2024/ANGGREK/F18-20240116-1000.png
  sat_day=`basename $file | cut -d- -f1,2 |sed -e 's/^T0/TROPICS0/'`  # F18-20240116, TROPICS06-20240130 
  # find all potentail matches in step7
  echo $file, hour=$hr
  echo --------------
  for sat_pc in /data1/web/lswg/group/m2m-merge-version4-20251202/step7-propogated-to-this-moment/$year/$event/${sat_day}-${hr}??.mat-*.png; do 
    #e.g., /data1/web/lswg/group/m2m-merge-version4-20251202/step7-propogated-to-this-moment/2024/YAGI/NOAA21-20240905-0545.mat-3.png
    sat_hr=`basename $sat_pc |cut -d- -f3 |sed -e 's/.mat//'`
    echo $sat_pc, $sat_hr
  done
  echo ================================================================== 
  echo

  # from this file 
  #  echo "<img src='$file'>" 
done
# > ../v2_still_bad.html


