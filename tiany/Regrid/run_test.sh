
 ./reproj ATMS raw_data/2A-CLIM.NPP.ATMS.GPROF2021v1.20230731-S061957-E080126.060919.V07B.HDF5 grid_2A-CLIM.NPP.ATMS.GPROF2021v1.20230731-S061957-E080126.060919.V07B.HDF5

grads -blc <<EOF
open new_test.ctl
set grads off 
set gxout grfill
d sti
cbarn 
printim grid_sti.png png white x1400 y1000
c
set gxout grfill
set grads off
set clevs 0.1 0.2 0.4 0.8 1.6 3.2 6.4 12.8 25.6
d sfcrain
cbarn 
printim grid_sfcrain.png png white x1400 y1000
quit
EOF

 convert -trim grid_sti.png trim_grid_sti.png
 convert -trim grid_sfcrain.png trim_grid_sfcrain.png

