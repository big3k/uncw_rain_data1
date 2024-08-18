
# Process every 3 lines 
#Processing case: /data1/youy/grid_satellites/noaa20-gprof-events-imager-propogated-final-land/202101/case_0033/sate_select_case_022.mat
#lon1=  -99.00 lon2=  -69.00 lat1=   20.00 lat2=   50.00 2021 1 1 7 17
#Using good MRMS file: /data1/tiany/MRMS/CONUS/PrecipRate_00.00/20210101/MRMS_PrecipRate_00.00_20210101-071800.grib2.gz.


nline=1
while read line; do 
if [ $nline -eq 1 ]; then 
   matf=$(echo $line | sed -e 's/Processing case: //') 
   n20_name=$(echo $matf |awk -F/ -vOFS=- '{print $6, $7}')  
   echo $matf
   echo $n20_name
   n20f=tmp/$n20_name
   matlab -batch "dump_a_case('$matf', '$n20f')" 
fi 
if [ $nline -eq 2 ]; then 
   read -r lab1 lon1 lab2 lon2 lab3 lat1 lab4 lat2 <<<$line
   echo $line
   echo $lon1 $lon2 $lat1 $lat2
   nx=$(echo $lon1 $lon2 |awk '{ print int(($2-$1)*10+1)}') 
   ny=$(echo $lat1 $lat2 |awk '{ print int(($2-$1)*10+1)}') 
   echo $nx $ny
fi 
if [ $nline -eq 3 ]; then 
   mrmsf=tmp/$(echo $line |grep -Po 'MRMS_PrecipRate_00.00_\K.*?(?=\.grib2.gz)').bin_sub
   echo $mrmsf
   # create ctl files 
   echo here: $lon1 $lon2 $lat1 $lat2 $nx $ny
   sed -e "s/nx__/$nx/" -e "s/ny__/$ny/" -e "s/lon1__/$lon1/" -e "s/lat1__/$lat1/" -e "s#mrmsf__#$mrmsf#" ctl_template/mrms_mat.ctl > case_mrms.ctl 
   sed -e "s/nx__/$nx/" -e "s/ny__/$ny/" -e "s/lon1__/$lon1/" -e "s/lat1__/$lat1/" -e "s#mrmsf__#$n20f#" ctl_template/mrms_mat.ctl > case_n20.ctl 
   mrms_name=`echo $mrmsf |cut -c5-19`

# compare MRMS and NOAA20 
grads -blc <<EOF
open case_mrms.ctl
set grads off
set gxout grfill
*set lat 12 15
*set lon 42 45
set mpdset hires
set grads off
set clevs 0.1 0.2 0.4 0.8 1.6 3.2 6.4 12.8 
d pr 
cbarn
draw title MRMS rain (mm/h) at 0.1-deg, 20210101-103800
printim plots/${mrms_name}.png png white x1400 y1000
c
open case_n20.ctl
set grads off
set mpdset hires
set clevs 0.1 0.2 0.4 0.8 1.6 3.2 6.4 12.8 
d pr.2 
cbarn
draw title N20: $n20_name
printim plots/${n20_name}.png png white x1400 y1000

quit

EOF

convert -trim plots/${mrms_name}.png plots/trim-${mrms_name}.png
convert -trim plots/${n20_name}.png plots/trim-${n20_name}.png
convert +append plots/trim-${mrms_name}.png plots/trim-${n20_name}.png compare_plots/${n20_name}.png

 # restart the 3-line group 
 nline=0
fi

let nline=nline+1

done < cases.log.main.txt

exit

