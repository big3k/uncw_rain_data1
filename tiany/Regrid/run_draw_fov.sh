
# run five sensors

./draw_fov ATMS 2A-CLIM.NPP.ATMS.GPROF2021v1.20230731-S061957-E080126.060919.V07B.HDF5
./draw_fov SSMIS 2A-CLIM.F17.SSMIS.GPROF2021v1.20181031-S121907-E140059.061867.V07A.HDF5
./draw_fov AMSR2 2A-CLIM.GCOMW1.AMSR2.GPROF2021v1.20231030-S091550-E105442.060913.V07B.HDF5
./draw_fov GMI 2A-CLIM.GPM.GMI.GPROF2021v1.20231030-S061042-E074315.054942.V07C.HDF5
./draw_fov MHS 2A-CLIM.METOPA.MHS.GPROF2021v1.20191223-S004008-E022128.068368.V07A.HDF5

for sensor in ATMS MHS SSMIS AMSR2 GMI; do 
  # create ctl file 
  cat > ${sensor}.ctl <<EOF 
DSET ^$sensor.2gd4r
*options template
TITLE Gird output
UNDEF -9999.0
XDEF  3600 LINEAR  -179.95 0.1
YDEF  1800 LINEAR  -89.95 0.1
ZDEF 1    LINEAR 1 1
TDEF 1    LINEAR 0Z01jan1991 30mn
VARS 2
sfcrain               1 99  **
sti                  1 99  **
ENDVARS
EOF


# generate 4 plots for each sensor 
#  get lat/lon box of each plot

 for gs in ${sensor}_*.gs; do 

 plotf=${gs/.gs/.png} 
 awk '{ for(i=2;i<NF;i+=2) print $i }' $gs |sort -n > /tmp/lons
 awk '{ for(i=3;i<NF;i+=2) print $i }' $gs |sort -n > /tmp/lats
 # ignore lon wrapping for now
 minlon=`head -1 /tmp/lons |awk '{print $1-0.5}'`
 maxlon=`tail -1 /tmp/lons |awk '{print $1+0.5}'`
 minlat=`head -1 /tmp/lats |awk '{print $1-0.5}'`
 maxlat=`tail -1 /tmp/lats |awk '{print $1+0.5}'`

 grads -blc <<EOF2
open $sensor.ctl 
set grads off
set lat $minlat $maxlat 
set lon $minlon $maxlon 
set gxout grfill
set clevs 0 0.25 0.5 1 2 4 8 16 32
d sfcrain
cbarn
draw title sfcrain
$gs
printim $plotf png white x1400 y1000
quit
EOF2

convert -trim $plotf trim-$plotf

 done  # gs

 
done # sensor


