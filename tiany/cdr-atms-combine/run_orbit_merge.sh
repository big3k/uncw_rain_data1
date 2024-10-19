#! /bin/bash 

# Merge scan files by orbit number (the "_bnnnnn_" field) in the .h5 file. 
# Algorithm description: 
# Given the start/end dates, first the first orbit and last orbit number, then loop through 
# the orbit numbers. For each orbit number, search all the files in the current day and next day, 
# except for the first day, wherein the first orbit number is searched in the first day files and in 
# the previous day files. 

# For each orbit, compile two files, each containing a list, one for all the GATMO h5 files, 
# the other TATMS files. The two list are supposed to have the same number of h5 files. 
# The files in each list shall be ordered by time values in the file names 
#  (which "sort" does it).  

# Sample locations of input files 
# /data1/youy/cdr-atms-combine/gatmo/2021/20210103/
# /data1/youy/cdr-atms-combine/tatms/2021/20210103/
#Output dir format example
# /tmp/2021/20210103/

# ===  User Configuration ========================================== 
#data location above yyyy/yyyymmdd/
daily_gatmo_dir="/data1/tiany/cdr-atms-combine/test_input/gatmo"
daily_tatms_dir="/data1/tiany/cdr-atms-combine/test_input/tatms"
daily_output_dir="/tmp/"   # make sure user has write permission 

sday=20120302  # start day
eday=20120303  # end day 

# ===  End User Configuration ===================================== 

tmpdir=/tmp/`whoami`  # for storing input file lists 
mkdir -p $tmpdir || { echo No write permission: $tmpdir; exit -1; }

# Find the starting and ending orbit number 
syear=$(date -u -d "$sday" +%Y)
s_orbit=`ls $daily_gatmo_dir/$syear/$sday/*.h5 |sort |head -1 | xargs basename |cut -d_ -f6`
eyear=$(date -u -d "$eday" +%Y)
e_orbit=`ls $daily_gatmo_dir/$eyear/$eday/*.h5 |sort |tail -1 | xargs basename |cut -d_ -f6`

s_orbit_num=`awk -va=${s_orbit:1} 'BEGIN{ printf "%d", a}'` # b01796 -> 01796 -> 1796
e_orbit_num=`awk -va=${e_orbit:1} 'BEGIN{ printf "%d", a}'` 

#echo $s_orbit $e_orbit 
#echo $s_orbit_num $e_orbit_num
first_step=1

while [ $s_orbit_num -le $e_orbit_num ]; do 

 orbit_in=`awk -va=$s_orbit_num 'BEGIN{ printf "b%5.5d\n", a}'` # b01796
 orbit_out=`awk -va=$s_orbit_num 'BEGIN{ printf "b%6.6d\n", a}'` # b001796,  orbit number in output filename

 if [ $first_step -eq 1 ]; then # search sday and previous day for the first orbit
   pday=`date -u -d "$sday -1 day" +%Y%m%d`
   pyear=`date -u -d "$sday -1 day" +%Y`
   cdate=$sday 
   cyear=$syear 
   # generate file lists for the orbit
   find -L $daily_gatmo_dir/$pyear/$pday/ $daily_gatmo_dir/$cyear/$cdate/  \
     -name "GATMO_npp*_${orbit_in}_*.h5" 2>/dev/null |sort > $tmpdir/gatmo_${orbit_in}.txt
   find -L $daily_tatms_dir/$pyear/$pday/ $daily_tatms_dir/$cyear/$cdate/ \
     -name "TATMS_npp*_${orbit_in}_*.h5" 2>/dev/null |sort > $tmpdir/tatms_${orbit_in}.txt
   orbit_date=`head -1 $tmpdir/gatmo_${orbit_in}.txt |xargs basename |cut -c12-19` #20120303
   orbit_year=`date -u -d "$orbit_date" +%Y`  # 2012
   first_step=0 # do this only once at beginning 
 else  # look forward one day
   pday=`date -u -d "$orbit_date +1 day" +%Y%m%d`
   pyear=`date -u -d "$orbit_date +1 day" +%Y`
   cdate=$orbit_date
   cyear=$orbit_year 
   # generate file lists for the orbit
   find -L $daily_gatmo_dir/$pyear/$pday/ $daily_gatmo_dir/$cyear/$cdate/  \
     -name "GATMO_npp*_${orbit_in}_*.h5" 2>/dev/null|sort > $tmpdir/gatmo_${orbit_in}.txt
   find -L $daily_tatms_dir/$pyear/$pday/ $daily_tatms_dir/$cyear/$cdate/ \
     -name "TATMS_npp*_${orbit_in}_*.h5" 2>/dev/null |sort > $tmpdir/tatms_${orbit_in}.txt
   orbit_date=`head -1 $tmpdir/gatmo_${orbit_in}.txt |xargs basename |cut -c12-19` #20120303
   orbit_year=`date -u -d "$orbit_date" +%Y`  # 2012
 fi

 # check to make sure the two files have matching data
 cat $tmpdir/gatmo_${orbit_in}.txt |xargs -n1 basename |cut -c11-45 > $tmpdir/time_gatmo_${orbit_in}.txt
 cat $tmpdir/tatms_${orbit_in}.txt |xargs -n1 basename |cut -c11-45 > $tmpdir/time_tatms_${orbit_in}.txt
 cmp -s $tmpdir/time_gatmo_${orbit_in}.txt $tmpdir/time_tatms_${orbit_in}.txt || \
 { echo orbit files for ${orbit_in} do not match; continue; }

 nfiles=`cat $tmpdir/gatmo_${orbit_in}.txt |wc -l` 
 # Each orbit's date is marked by the date of the first h5 file 
 echo $orbit_in, $orbit_date, $orbit_year, $tmpdir/gatmo_${orbit_in}.txt, \
      $tmpdir/tatms_${orbit_in}.txt, nfiles=$nfiles
 
 stime=`head -1 $tmpdir/time_gatmo_${orbit_in}.txt |cut -c12-18`
 etime=`tail -1 $tmpdir/time_gatmo_${orbit_in}.txt |cut -c21-27`
 #now run merge
 mkdir -p $daily_output_dir/$orbit_year/$orbit_date/ 
 outfile=$daily_output_dir/$orbit_year/$orbit_date/NPP_ATMS_d${orbit_date}_t${stime}_e${etime}_${orbit_out}.nc
 # ./merge_npp_atms_daily $tmpdir/gatmo_${orbit_in}.txt $tmpdir/tatms_${orbit_in}.txt $outfile
  
 let s_orbit_num=s_orbit_num+1  # move on to next orbit

done  # all the orbits are done 

