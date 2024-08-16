# Hi-res MRS
grads -blc <<EOF
open PrecipRate.ctl
set grads off
set gxout grfill
*set lat 12 15
*set lon 42 45
set mpdset hires
set grads off
set clevs 0.1 0.2 0.4 0.8 1.6 3.2 6.4 12.8 25.6 51.2 102.4
d pr 
cbarn
draw title MRMS rain (mm/h) at 0.01-deg, 20240708-050800 
printim beryl-0.01.png png white x1400 y1000
c
set grads off
set lat 20 35 
set lon 260 275
set mpdset hires
set clevs 0.1 0.2 0.4 0.8 1.6 3.2 6.4 12.8 25.6 51.2 102.4
d pr 
cbarn
draw title MRMS rain (mm/h) at 0.01-deg, 20240708-050800 
printim zoom-beryl-0.01.png png white x1400 y1000

reinit
open low_res.ctl
set grads off
set gxout grfill
*set lat 12 15
*set lon 42 45
set mpdset hires
set grads off
set clevs 0.1 0.2 0.4 0.8 1.6 3.2 6.4 12.8 25.6 51.2 102.4
d pr
cbarn
draw title MRMS rain (mm/h) at 0.1-deg, 20240708-050800
printim beryl-0.1.png png white x1400 y1000
c
set grads off
set lat 20 35
set lon 260 275
set mpdset hires
set clevs 0.1 0.2 0.4 0.8 1.6 3.2 6.4 12.8 25.6 51.2 102.4
d pr
cbarn
draw title MRMS rain (mm/h) at 0.1-deg, 20240708-050800
printim zoom-beryl-0.1.png png white x1400 y1000


quit
EOF

convert -trim beryl-0.01.png trim_beryl-0.01.png
convert -trim zoom-beryl-0.01.png trim_zoom-beryl-0.01.png
convert -trim beryl-0.1.png trim_beryl-0.1.png
convert -trim zoom-beryl-0.1.png trim_zoom-beryl-0.1.png

