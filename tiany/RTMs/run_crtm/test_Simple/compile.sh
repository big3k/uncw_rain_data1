libroot="/data1/tiany/RTMs/crtm/src/Build/crtm_v2.4.0-alpha"
crtm_inc="${libroot}/include" 
crtm_lib="${libroot}/lib" 

echo "-L${crtm_lib}" 

gfortran -o test_Simple test_Simple.f90 -I${crtm_inc} -I/usr/lib64/gfortran/modules  -I/usr/include -L${crtm_lib} -L/usr/lib64 -lcrtm -lnetcdff -lgomp 



exit
gfortran -O2 -g  -I${crtm_inc} -I/usr/lib64/gfortran/modules  -I/usr/include -L${crtm_lib} -L/usr/lib64 -lcrtm -lnetcdff \
-o test_Simple test_Simple.f90



