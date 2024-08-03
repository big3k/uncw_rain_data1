#! /usr/bin/bash

# Usage:
# $0 [start_yyyymmdd end_yyyymmdd]
# User enter start_date and end_date (yyyymmdd). If not given, use the week before till yesterday. 

# Note: 
#  -- There is a data volume limit of 250 Gigabytes per request. 
#  -- Each daily file can be ~12G in size. So try not to request more than 15 days each time. 


# Script to download GFS data in the date range. Untar each tar and delete files not needed. 
# ------------------ config params
wait_sec=30  # in seconds. Intervals to check if files are staged. 
tar_dir=/data1/tiany/GFS_auto_download/raw_tars # dir to save the downloaded tar files. 
# ------------------ end config params

if [ $# -eq 2 ]; then
 start_day=$1
 end_day=$2
else
 end_day=$(date -d "yesterday" +%Y%m%d)
 start_day=$(date -d "$end_day -6 day" +%Y%m%d)
fi

echo $start_day
echo $end_day
#--------------------------------------

ssec=`date -d "$start_day" +%s`
esec=`date -d "$end_day 23:59:59" +%s`

nday=`awk "BEGIN{ print int(($esec-$ssec)/(24*60*60)+0.5) }"`

echo $nday

if [ $nday -gt 15 ]; then 
  echo "The request range can not be larger than 15 days. Quit ..." 
  exit -1
fi

# reformat date ranges
begyear=`date -d "$start_day" +%Y`
begmonth=`date -d "$start_day" +%m`
begday=`date -d "$start_day" +%d`
endyear=`date -d "$end_day" +%Y`
endmonth=`date -d "$end_day" +%m`
endday=`date -d "$end_day" +%d`

echo $begyear $begmonth $begday
echo $endyear $endmonth $endday

#8/1/2024
# How it works: 

# Step 1: submit the request

req_id=$$

curl -X POST \
   -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/png,image/svg+xml,*/*;q=0.8" \
   -d "satdisptype=N%2FA&stations=00&station_lst=&typeofdata=MODEL&dtypelist=&begdatestring=&enddatestring=&begyear=$begyear&begmonth=$begmonth&begday=$begday&beghour=&begmin=&endyear=$endyear&endmonth=$endmonth&endday=$endday&endhour=&endmin=&outmed=FTP&outpath=&pri=500&datasetname=GFS3&directsub=Y&emailadd=jianxiac%40gmail.com&outdest=FILE&applname=&subqueryby=STATION&tmeth=Awaiting-Data-Transfer" \
  https://www.ncei.noaa.gov/has/HAS.FileSelect  > response_${req_id}.txt


# The response will contain the "Order Number" field: 
#<tr><td class="var">Order Number:</td><td class="val">HAS012540680<a href="HAS.CheckOrderStatus?hasreqid=HAS012540680&emailadd=jianxiac@gmail.com" class="button completeButton statusButton noprint floatRight">Check order status</a></td></tr>
#
# And when the data are ready, they will be located at as daily tar files: 
# https://www.ncei.noaa.gov/pub/has/model/HAS012540678/

orderN=`grep "Order Number" response_${req_id}.txt | grep -Po 'hasreqid=\K.*?(?=&emailadd=)'`

orderURL="https://www.ncei.noaa.gov/pub/has/model/$orderN/" 
echo 
echo "Trying URL: $orderURL" 

# check if files are staged
# checking the URL to see if: it is there, and the number of files matches the number of days 
sleep $wait_sec
while : ; do  # wait for orderURL to be ready 
  wget -O ${orderN}_${req_id}.html $orderURL
  if [ $? -eq 0 ]; then 
    #parse out file names  
    grep "\.tar" ${orderN}_${req_id}.html |grep -Po '\.tar">\K.*?(?=</a>)' > files_$orderN.txt
    nfiles=`cat files_${orderN}.txt |wc -l`
    if [ $nfiles -eq $nday ]; then 
       echo "All files are ready. Go download them ..." 
       break
    fi
    echo "Files not ready. Waiting ..." 
    sleep $wait_sec # wait for all files to stage
  fi
  echo "URL or Files not ready. Waiting ..." 
  sleep $wait_sec  # wait for order URL 
done 

# Now download each file  
# wait a little extra, to be safe
sleep $wait_sec  

cat files_${orderN}.txt  |while read filename; do 
  echo Downloading $orderURL/$filename
  wget -nv -O $tar_dir/$filename $orderURL/$filename
  # unpack and delete files not needed. 
done








