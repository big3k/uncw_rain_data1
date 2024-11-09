#! /bin/bash
# Function: Extract AVN fields from GFS file for the given day range. 
# Note: need to process one extra day to the end, b/c eveyday's 24-45Z need to be 
#  present which is next day's 00-21Z 
# Usage:  $0 

. ./config

# input files example: 
# $gfs_raw_dir/20240830/gfs_3_20240830_0000_000.grb2
# $gfs_raw_dir/20240830/gfs_3_20240830_0000_003.grb2
# $gfs_raw_dir/20240830/gfs_3_20240830_0600_000.grb2
# $gfs_raw_dir/20240830/gfs_3_20240830_0600_003.grb2
# $gfs_raw_dir/20240830/gfs_3_20240830_1200_000.grb2
# $gfs_raw_dir/20240830/gfs_3_20240830_1200_003.grb2
# $gfs_raw_dir/20240830/gfs_3_20240830_1800_000.grb2
# $gfs_raw_dir/20240830/gfs_3_20240830_1800_003.grb2

# output files example: 
# $top_dir/input/proc_avn/avn_u00_2024243.bin
# $top_dir/input/proc_avn/avn_u03_2024243.bin
# $top_dir/input/proc_avn/avn_u06_2024243.bin
# $top_dir/input/proc_avn/avn_u09_2024243.bin
# $top_dir/input/proc_avn/avn_u12_2024243.bin
# $top_dir/input/proc_avn/avn_u15_2024243.bin
# $top_dir/input/proc_avn/avn_u18_2024243.bin
# $top_dir/input/proc_avn/avn_u21_2024243.bin

[ $start_day -le $end_day ] || { echo Start day is later than end day!; exit -1; } 

new_end_day=`date -u -d "$end_day +1 day" +%Y%m%d` # do an extra day

iday=$start_day
while [ $iday -le $new_end_day ]; do 

yyyymmdd=$iday
pday=`date -u -d "$iday -1 day" +%Y%m%d`
yyyyddd=`date -u -d "$yyyymmdd" +%Y%j`   # not used anymore

mkdir -p $top_dir/input/proc_avn/$iday 

 for gfile in $gfs_raw_dir/$yyyymmdd/gfs*_${yyyymmdd}_*.grb2; do 
  echo Processing $gfile 
  bname=`basename $gfile` # -> gfs_3_20240830_0600_003.grb2
  rhh=`echo $bname |cut -d_ -f4 |cut -c1-2`  # 06
  fhh=`echo $bname |cut -d_ -f5 |cut -d. -f1`  # 003 
  zhh=`awk -vrhh=$rhh -vfhh=$fhh 'BEGIN {printf "%2.2d\n", rhh+fhh}'` 

  wgrib2 ${gfile} -s | grep ":UGRD:"  | grep ":10 m above ground:"  | wgrib2 -i ${gfile}  -no_header -order we:ns -bin $top_dir/input/proc_avn/$iday/avn_u${zhh}.bin
  wgrib2 ${gfile} -s | grep ":VGRD:"  | grep ":10 m above ground:"  | wgrib2 -i ${gfile}  -no_header -order we:ns -bin $top_dir/input/proc_avn/$iday/avn_v${zhh}.bin
  wgrib2 ${gfile} -s | grep ":PWAT:"  | grep ":entire atmosphere (considered as a single layer):" | wgrib2 -i ${gfile}  -no_header -order we:ns -bin $top_dir/input/proc_avn/$iday/avn_tpw${zhh}.bin
  wgrib2 ${gfile} -s | grep ":TMP:"   | grep ":surface:"          | wgrib2 -i ${gfile}  -no_header -order we:ns -bin $top_dir/input/proc_avn/$iday/avn_ts${zhh}.bin
  wgrib2 ${gfile} -s | grep ":TMP:"   | grep ":2 m above ground:" | wgrib2 -i ${gfile}  -no_header -order we:ns -bin $top_dir/input/proc_avn/$iday/avn_ts2m${zhh}.bin

 done 

   # replicate 00Z-21Z as previousday's 24-45Z
   for hr in {00..21..3}; do 
     phr=`awk -vhr=$hr 'BEGIN{ printf "%2.2d\n", hr+24}'`
     for fnm in avn_tpw avn_ts avn_ts2m avn_u avn_v; do
       ln -s $top_dir/input/proc_avn/$iday/${fnm}${hr}.bin \
             $top_dir/input/proc_avn/$pday/${fnm}${phr}.bin 
     done
   done
 
 iday=`date -u -d "$iday +1 day" +%Y%m%d`
done

exit

