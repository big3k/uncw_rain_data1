
******************* This funtion draws a polygon in lat/lon coordinates 
* Usage:
* plot_poly lon1 lat1 lon1 lat2 ... lonN latN 

function plot_poly(arg)

xys=''
id=1
lon1=subwrd(arg,id)
lat1=subwrd(arg,id+1)

while ( lat1 != '' ) 
  id=id+2
  'q w2xy 'lon1' 'lat1
  x1=subwrd(result,3)
  y1=subwrd(result,6)
  xys=xys' 'x1' 'y1
  lon1=subwrd(arg,id)
  lat1=subwrd(arg,id+1)

endwhile 

'draw line 'xys 
