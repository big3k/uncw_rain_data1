#! /usr/bin/bash

# Automatically download gprof data. 
# Change start and end date: 
#--------------------------------------
start_day=20210101
end_day=20210104
#end_day=20211231
#--------------------------------------

ssec=`date -d "$start_day" +%s`
esec=`date -d "$end_day" +%s`

nday=`awk "BEGIN{ print ($esec-$ssec)/(24*60*60) }"`

echo $nday

# download every day
for day in `seq 0 $nday`; do 
 web_date=`date -d "$start_day + $day day" +%Y/%m/%d` 
 local_date=`date -d "$start_day + $day day" +%Y%m%d` 
 echo $web_date, $local_date

 mkdir -p $local_date/gprof 

 #get index file for the day
 wget -O logs/index.${local_date}.txt -o logs/${local_date}.log  --user=youy@uncw.edu --password=youy@uncw.edu -r -np -nd https://arthurhouhttps.pps.eosdis.nasa.gov/gpmdata/$web_date/gprof/
 # parse file names 
 grep HDF5 logs/index.${local_date}.txt |grep -Po 'href="\K(.*?)(?=">)' |while read hdf5; do
   echo Downloading $hdf5
   wget -O $local_date/gprof/$hdf5 -o logs/download_${hdf5}.log --user=youy@uncw.edu --password=youy@uncw.edu -r -np -nd https://arthurhouhttps.pps.eosdis.nasa.gov/gpmdata/$web_date/gprof/$hdf5 
 done

done

exit

