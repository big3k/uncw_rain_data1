#! /usr/bin/bash
# 7/29/2026: 
# Some of the mat files downloaded/subsetted by students have "CLIM-PA" instead of 
# "CLIM" in the file name: e.g., 
# 
# /data1/youy/satellites/gprof-v08/NPP/202209/2A-CLIM-PA.NPP.ATMS.GPROFNNv1.20220930-S195131-E213300.056614.V08A.nc.mat
# 
# So remove '-PA' if there exists before comparing with server file list.  
# 
# Check any missing gprof files and re-download them 
#
# Currently there are two steps on Yalei's part: 

# 1. Download raw gprof nc files into daily dirs like the following: 
# /data2/satellites/zz_download_gprof/20260110/
#
# 2. Extract useful data and discard the origninal nc files. The extracted 
#  data are saved in separate directories as .mat files like this: 
# /data1/youy/satellites/gprof-v08/NPP/202501/2A-CLIM.NPP.ATMS.GPROFNNv1.20250131-S125246-E143415.068726.V08A.nc.mat

# This script will check the server, get the daily list of files, and 
# compare the list of existing .mat files. If there are correspo;nding .mat 
# files missing, download the original .nc files and let Yalei to the extraction 
# again to create the missing .mat files.  

mkdir logs
#--------------------------------------
start_day=19870101
end_day=20251231
#--------------------------------------

ssec=`date -d "$start_day" +%s`
esec=`date -d "$end_day" +%s`

nday=`awk "BEGIN{ print ($esec-$ssec)/(24*60*60) }"`

echo $nday

# download every day
for day in `seq 0 $nday`; do 
 web_date=`date -d "$start_day + $day day" +%Y/%m/%d` 
 local_date=`date -d "$start_day + $day day" +%Y%m%d` 
 local_ym=`date -d "$start_day + $day day" +%Y%m` 
 echo $web_date, $local_date

 mkdir -p $local_date/gprof 

while true; do
 #get index file for the day
 wget -O logs/index.${local_date}.txt -o logs/${local_date}.log  --user=youy@uncw.edu --password=youy@uncw.edu -r -np -nd https://arthurhouhttps.pps.eosdis.nasa.gov/gpmdata/$web_date/gprof/
  status=$?
  if [ $status -eq 4 ]; then
    echo Network issue ...
    sleep 2 # wait a bit to retry
  else
   break
  fi
done


 # parse file names
grep 2A-CLIM logs/index.${local_date}.txt |grep -Po 'href="\K(.*?)(?=">)' |sort > logs/files_on_server.${local_date}.txt 

# get .mat file names 
ls /data1/youy/satellites/gprof-v08/*/$local_ym/*.${local_date}-*.mat |xargs -n1 basename |sed -e 's/.mat$//' -e 's/2A-CLIM-PA/2A-CLIM/' |sort > logs/existing_mat_files.${local_date}.txt 

# find what's missing and download 
comm -23 logs/files_on_server.${local_date}.txt logs/existing_mat_files.${local_date}.txt |while read hdf5; do
   echo Downloading $hdf5

  while true; do
    wget -O $local_date/gprof/$hdf5 -o logs/download_${hdf5}.log --user=youy@uncw.edu --password=youy@uncw.edu -r -np -nd https://arthurhouhttps.pps.eosdis.nasa.gov/gpmdata/$web_date/gprof/$hdf5
  status=$?
  if [ $status -eq 4 ]; then
    echo Network issue ...
    sleep 2 # wait a bit to retry
  else
   break
  fi

 done  # while true

done  # while read hdf5

done  # for day 

exit

