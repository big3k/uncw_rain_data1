#! /usr/bin/bash
# process gprof swath data: reproject to 0.1x0.1-deg global

exe=/data1/tiany/Regrid/reproj

in_dir=/data2/satellites
out_dir=/data1/tiany/grid_satellites

for sat in \
npp-gprof \
amsr2-gprof \
f16-gprof \
f17-gprof \
f18-gprof \
f19-gprof \
gmi-gprof \
metopa-gprof \
metopb-gprof \
metopc-gprof \
noaa18-gprof \
noaa19-gprof \
noaa20-gprof \
noaa21-gprof 
do 
 echo Doing $sat

 for dir_ym in $in_dir/$sat/20????; do 
   echo $dir_ym
   yyyymm=`basename $dir_ym`
   mkdir -p $out_dir/$sat/$yyyymm

   for hdf5 in $dir_ym/2A-*.HDF5; do 
     echo Processing $hdf5 ...
     base_inf=`basename $hdf5`
     outf=grid_$base_inf
     sensor=`echo $base_inf |cut -d\. -f3`
     $exe $sensor $hdf5 $out_dir/$sat/$yyyymm/$outf 
   done

  done

done



