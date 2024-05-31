
 ./reproj ATMS raw_data/2A-CLIM.NPP.ATMS.GPROF2021v1.20230731-S061957-E080126.060919.V07B.HDF5 grid_2A-CLIM.NPP.ATMS.GPROF2021v1.20230731-S061957-E080126.060919.V07B.HDF5

grads -blc <<EOF
open new_test.ctl
set grads off 
set gxout grfill
d sti
cbarn 
printim grid_sti.png png white x1400 y1000
quit
EOF


