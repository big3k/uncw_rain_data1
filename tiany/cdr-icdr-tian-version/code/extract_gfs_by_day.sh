#! /bin/bash
# Function: Extract 1-day's worth of AVN fields from GFS file. 
# Usage:  $0 yyyymmdd
# Ex:     $0 20120515 

. ../config

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

[[ $# -eq 1 ]] || { echo Missing yyyymmdd!; exit -1; } 

yyyymmdd=$1
yyyyddd=`date -u -d "$yyyymmdd" +%Y%j` 

mkdir -p $top_dir/input/proc_avn/ 

for gfile in $gfs_raw_dir/$yyyymmdd/gfs*_${yyyymmdd}_*.grb2; do 
  echo Processing $gfile 
  bname=`basename $gfile` # -> gfs_3_20240830_0600_003.grb2
  rhh=`echo $bname |cut -d_ -f4 |cut -c1-2`  # 06
  fhh=`echo $bname |cut -d_ -f5 |cut -d. -f1`  # 003 
  zhh=`awk -vrhh=$rhh -vfhh=$fhh 'BEGIN {printf "%2.2d\n", rhh+fhh}'` 

  wgrib2 ${gfile} -s | grep ":UGRD:"  | grep ":10 m above ground:"  | wgrib2 -i ${gfile}  -no_header -order we:ns -bin $top_dir/input/proc_avn/avn_u${zhh}_$yyyyddd.bin
  wgrib2 ${gfile} -s | grep ":VGRD:"  | grep ":10 m above ground:"  | wgrib2 -i ${gfile}  -no_header -order we:ns -bin $top_dir/input/proc_avn/avn_v${zhh}_$yyyyddd.bin
  wgrib2 ${gfile} -s | grep ":PWAT:"  | grep ":entire atmosphere (considered as a single layer):" | wgrib2 -i ${gfile}  -no_header -order we:ns -bin $top_dir/input/proc_avn/avn_tpw${zhh}_$yyyyddd.bin
  wgrib2 ${gfile} -s | grep ":TMP:"   | grep ":surface:"          | wgrib2 -i ${gfile}  -no_header -order we:ns -bin $top_dir/input/proc_avn/avn_ts${zhh}_$yyyyddd.bin
  wgrib2 ${gfile} -s | grep ":TMP:"   | grep ":2 m above ground:" | wgrib2 -i ${gfile}  -no_header -order we:ns -bin $top_dir/input/proc_avn/avn_ts2m${zhh}_$yyyyddd.bin

done 

exit

