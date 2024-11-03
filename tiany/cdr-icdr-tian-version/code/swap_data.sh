#! /bin/bash
# Function: swap endian-ness of 1B data 
# Usage:  $0  sensor_name<amsua|mhs> sat_name<metop-a|noaa-18|noaa-19>
# Example: $0 amsua noaa-18
[ $# -eq 2 ] || { echo "Usage: $0 sensor_name sat_name"; exit -1; } 

. ../config

sensor=$1
sat=$2
fsensor=${sensor_fname[$sensor]} # e.g., amsua->AMAX
fsat=${sat_fname[$sat]} # e.g., noaa-19 -> NP 
struct_f=${struct_fname[$sensor]} 
# e.g., /data1/tiany/cdr-icdr-tian-version//code/swap_endian/Structure_AMSUA_1b

# start_day and end_day defined in config. 

# Construct list of files to be converted 
#---------------------------------------------------------------------
# Top level dir of satallite raw data:
# Example file:
# /data2/cdr/unswapped/amsua/aug2024/NSS.AMAX.NN.D24232.S1718.E1909.B9922425.SV
# /data2/cdr/unswapped/mhs/aug2024/NSS.MHSX.NP.D24244.S0200.E0352.B8022122.SV
# sat_raw_dir=/data2/cdr/unswapped/

flist_dir=$tmpdir/$sensor/$sat/
mkdir -p $flist_dir 

iday=$start_day
while [ $iday -le $end_day ]; do 

 yyyymmdd=$iday
 mmmyyyy=`date -u -d "$iday" +%b%Y |tr [A-Z] [a-z]`  # aug2024
 Dyyddd=`date -u -d "$iday" +D%y%j` # D24244 

 # swap one day at a time, to keep input and output directories in sync
 flist_f=$flist_dir/swap_flist_${iday}.txt
 ls $sat_raw_dir/$sensor/$mmmyyyy/NSS.${fsensor}.${fsat}.${Dyyddd}.* > $flist_f 
 out_dir=$top_dir/input/swapped/$sensor/$sat/$yyyymmdd/ 
 mkdir -p $out_dir
 $top_dir/code/swap_endian/swap_endian_1b.pl -s $struct_f -f $flist_f -d $out_dir 

 iday=`date -u -d "$iday +1 day" +%Y%m%d`

done



