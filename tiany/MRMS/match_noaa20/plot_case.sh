# compare MRMS and NOAA20 
grads -blc <<EOF
open mrms_mat.ctl
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
printim mrms_20210101-103800.png png white x1400 y1000
c
open noaa20.ctl
set grads off
set mpdset hires
set clevs 0.1 0.2 0.4 0.8 1.6 3.2 6.4 12.8 
d pr.2 
cbarn
draw title N20: 202101/case_0048/sate_select_case_028 
printim noaa20_20210101-103800.png png white x1400 y1000

quit
EOF

exit
convert -trim beryl-0.01.png trim_beryl-0.01.png
convert -trim zoom-beryl-0.01.png trim_zoom-beryl-0.01.png
convert -trim beryl-0.1.png trim_beryl-0.1.png
convert -trim zoom-beryl-0.1.png trim_zoom-beryl-0.1.png

