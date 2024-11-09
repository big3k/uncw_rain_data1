#! /bin/bash
# Function: process ccdr for given satellite and sensor 
# Usage:  $0  sensor_name<amsua|mhs> sat_name<metop-a|noaa-18|noaa-19>
# Example: $0 amsua noaa-18

[ $# -eq 2 ] || { echo "Usage: $0 sensor_name sat_name"; exit -1; }

sensor=$1
sat=$2

. ./config

[ $start_day -le $end_day ] || { echo Start day is later than end day!; exit -1; } 

# run tcdr, day by day

iday=$start_day
while [ $iday -le $end_day ]; do 

  yyyymmdd=$iday

  echo TCDR Processing $yyyymmdd
  # FCDR file list for ama2nc 
   input_dir=$top_dir/output/fcdr-pre/$sensor/$sat/$yyyymmdd/
  # GFS input file dir 
   gfs_input_dir=$top_dir/input/proc_avn/$yyyymmdd/
   # temp solution for overcoming the long directory name issue: 
   # runama will crash if input_dir and output_dir are too long
   n_gfs_files=`ls $gfs_input_dir/*.bin |wc -l`

   if [ "X$sensor" == "Xamsua" ]; then
     cd $top_dir/code/amsua-tcdr-pre/ama2nc
     out_dir=$top_dir/output/tcdr-pre/$sensor/$sat/$yyyymmdd/
     mkdir -p $out_dir
     ln -snf $input_dir tmp_input_dir
     ln -snf $gfs_input_dir tmp_gfs_dir
     ln -snf $out_dir tmp_output_dir
     ls $input_dir > amafile
       if [ $n_gfs_files -ge 80 ]; then  # don't process if GFS has missing files
         time ./runama tmp_input_dir/ tmp_gfs_dir/ tmp_output_dir/ > ama2nc.log 2> ama2nc.error
       else 
         echo "Missing GFS files for $iday. Expected: 80. Availble: $n_gfs_files" 
       fi 
   elif [ "X$sensor" == "Xmhs" ]; then
     cd $top_dir/code/amsub-tcdr-pre/amb2nc
     out_dir=$top_dir/output/tcdr/$sensor/$sat/$yyyymmdd/ # final prod, no "-pre"
     mkdir -p $out_dir
     # need mhs fcdr ($input_dir), amsua fcdr, amsua tcdr, and amsua 1b (.SV) 
     amsua_fcdr_dir=$top_dir/output/fcdr-pre/amsua/$sat/$yyyymmdd/
     amsua_tcdr_dir=$top_dir/output/tcdr-pre/amsua/$sat/$yyyymmdd/
     amsua_1b_dir=$top_dir/input/swapped/amsua/$sat/$yyyymmdd/
     ln -snf $input_dir tmp_input_dir
     ln -snf $gfs_input_dir tmp_gfs_dir
     ln -snf $out_dir tmp_output_dir
     ln -snf $amsua_fcdr_dir tmp_amsua_fcdr 
     ln -snf $amsua_tcdr_dir tmp_amsua_tcdr 
     ln -snf $amsua_1b_dir tmp_amsua_1b 
     ln -snf $input_dir tmp_mhs_fcdr
     # get file list 
     ls $amsua_fcdr_dir > tmpA.list  # AMSUA FCDR
     ls $input_dir > tmpB.list  # MHS FCDR
     wc tmpA.list > fileA.length
     wc tmpB.list > fileB.length
     ./getfname > getfname.log

       if [ $n_gfs_files -ge 80 ]; then  # don't process if GFS has missing files
         time ./runamb tmp_amsua_fcdr/ tmp_amsua_1b/ tmp_amsua_tcdr/ tmp_mhs_fcdr/ \
                   tmp_gfs_dir/ tmp_output_dir/ > mhs2nc.log 2> mhs2nc.error 
       else 
         echo "Missing GFS files for $iday. Expected: 80. Availble: $n_gfs_files" 
       fi 
    else
     echo "wrong sensor name: $1. exit."
    exit -1
   fi
   rm tmp_input_dir tmp_output_dir

   iday=`date -u -d "$iday +1 day" +%Y%m%d`
done

exit

