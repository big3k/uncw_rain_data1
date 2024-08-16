#! /usr/bin/bash

# Usage:
# $0 [start_yyyymmdd end_yyyymmdd]
# User enter start_date and end_date (yyyymmdd). If not given, use the week before till yesterday. 

# What it does: 
# Download dailly GFS fx at 00, 06, 12, 18Z tar files (four per day). 
#  Unpack each tar file and only keep *_000.grb2 and *_003.grb2 files.  

#8/15/2024
# v2: if total wait time is over 1 hour, don't wait any more and download whatever is staged. 
#     This is because some files are missing, e.g.,  gfs_3_2005042906.tar

# Note: 
#  -- There is a data volume limit of 250 Gigabytes per request. 
#  -- Each daily file can be ~12G in size, for each of the 00, 06, 12, and 18Z fx time. So each day the total 
#     size can be up to ~50G. So the given date range is cut into 5-day chunks, with each chunk as a separate order. 

# Script to download GFS data in the date range. Untar each tar and delete files not needed. 
# ------------------ config params
wait_sec=30  # in seconds. Intervals to check if files are staged. 
wait_limit=3600 # total seconds to wait before downloading whatever is given, b/c there are missing files. 
tar_dir=/data1/tiany/GFS_auto_download/raw_tars # dir to save the downloaded tar files. 
unpack_dir=/data1/tiany/GFS_auto_download/GFS_00-03 # dir to contain unpacked/weeded files 
html=/data1/tiany/GFS_auto_download/html  #dir to store the responses from the web server
# ------------------ end config params

if [ $# -eq 2 ]; then
 start_day0=$1
 end_day0=$2
else
 end_day0=$(date -d "yesterday" +%Y%m%d)
 start_day0=$(date -d "$end_day0 -6 day" +%Y%m%d)
fi

echo $start_day0
echo $end_day0
#--------------------------------------

# cut day range into 5-day chunks, and put an order for each chunk
start_day=$start_day0

#===================== Grand Loop of 5-Day Chunks ============================
while [ $start_day -le $end_day0 ]; do 
  end_day=$(date -d "$start_day +4 day" +%Y%m%d)  # 5-day chunk
  if [ $end_day -gt $end_day0 ]; then  # yyyymmdd can be compared numerically 
    end_day=$end_day0
  fi

  ssec=`date -d "$start_day" +%s`
  esec=`date -d "$end_day 23:59:59" +%s`
  nday=`awk "BEGIN{ print int(($esec-$ssec)/(24*60*60)+0.5) }"`
  let nfiles=nday*4 # total files expected. 
  echo "$start_day  ==> $end_day, total days: $nday, total files expected: $nfiles" 

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
   -d "satdisptype=N%2FA&stations=00&stations=06&stations=12&stations=18&station_lst=&typeofdata=MODEL&dtypelist=&begdatestring=&enddatestring=&begyear=$begyear&begmonth=$begmonth&begday=$begday&beghour=&begmin=&endyear=$endyear&endmonth=$endmonth&endday=$endday&endhour=&endmin=&outmed=FTP&outpath=&pri=500&datasetname=GFS3&directsub=Y&emailadd=jianxiac%40gmail.com&outdest=FILE&applname=&subqueryby=STATION&tmeth=Awaiting-Data-Transfer" \
  https://www.ncei.noaa.gov/has/HAS.FileSelect  > $html/response_${req_id}.txt

# Request for all the 4 fx times: 00, 06, 12, and 18: 
#satdisptype=N%2FA&stations=00&stations=06&stations=12&stations=18&station_lst=&typeofdata=MODEL&dtypelist=&begdatestring=&enddatestring=&begyear=2005&begmonth=08&begday=02&beghour=&begmin=&endyear=2005&endmonth=08&endday=03&endhour=&endmin=&outmed=FTP&outpath=&pri=500&datasetname=GFS3&directsub=Y&emailadd=jianxiac%40gmail.com&outdest=FILE&applname=&subqueryby=STATION&tmeth=Awaiting-Data-Transfer


# The response will contain the "Order Number" field: 
#<tr><td class="var">Order Number:</td><td class="val">HAS012540680<a href="HAS.CheckOrderStatus?hasreqid=HAS012540680&emailadd=jianxiac@gmail.com" class="button completeButton statusButton noprint floatRight">Check order status</a></td></tr>
#
# And when the data are ready, they will be located at as daily tar files: 
# https://www.ncei.noaa.gov/pub/has/model/HAS012540678/

orderN=`grep "Order Number" $html/response_${req_id}.txt | grep -Po 'hasreqid=\K.*?(?=&emailadd=)'`

orderURL="https://www.ncei.noaa.gov/pub/has/model/$orderN/" 
echo 
echo "Trying URL: $orderURL" 

# check if files are staged
# checking the URL to see if: it is there, and the number of files matches the number of days 
sleep $wait_sec
total_wait=0
while : ; do  # wait for orderURL to be ready 
  /usr/bin/wget -O $html/${orderN}_${req_id}.html $orderURL
  if [ $? -eq 0 ]; then 
    #parse out file names  
    grep "\.tar" $html/${orderN}_${req_id}.html |grep -Po '\.tar">\K.*?(?=</a>)' > $html/files_$orderN.txt
    nsee=`cat $html/files_${orderN}.txt |wc -l`
    if [ $nsee -eq $nfiles -o $total_wait -ge $wait_limit ]; then 
       echo "All files are ready. Go download them ..." 
       break
    fi
    echo "Files not ready. Waiting ..." 
    let total_wait=total_wait+wait_sec+wait_sec
    sleep $wait_sec # wait for all files to stage
  fi
  echo "URL or Files not ready. Waiting ..." 
  sleep $wait_sec  # wait for order URL 
done 

# Now download each file  
# wait a little extra, to be safe
sleep $wait_sec  

cat $html/files_${orderN}.txt  |while read filename; do 
  echo Downloading $orderURL/$filename
  /usr/bin/wget -nv -O $tar_dir/$filename $orderURL/$filename
  fdate=`basename $tar_dir/$filename |cut -d'_' -f3 |cut -c-8` # parse out yyyymmdd
  mkdir -p $unpack_dir/$fdate # may already exist 
  # unpack and delete files not needed. 
  tar -xf $tar_dir/$filename -C $unpack_dir/$fdate/ --wildcards --no-anchored '*00[03].gr??'
  rm $tar_dir/$filename # delete raw tar file afterward
  
done

  # move ahead to next chunk 
  start_day=$(date -d "$end_day +1 day" +%Y%m%d)
done
#===================== End of Grand Loop of 5-Day Chunks ============================
echo "Done" 
exit
