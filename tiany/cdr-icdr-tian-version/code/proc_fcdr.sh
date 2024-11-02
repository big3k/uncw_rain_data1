#! /bin/bash
# Function: process fcdr for given satellite and sensor 
# Usage:  $0  sensor_name<amsua|mhs> sat_name<metop-a|noaa-18|noaa-19>
# Example: $0 amsua noaa-18

. ../config

[ $start_day -le $end_day ] || { echo Start day is later than end day!; exit -1; } 

# swap endian of the 1B data
$top_dir/code/swap_data.sh $sensor $sat 

exit


iday=$start_day
while [ $iday -le $end_day ]; do 

yyyymmdd=$iday
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
 iday=`date -u -d "$iday +1 day" +%Y%m%d`
done

exit

