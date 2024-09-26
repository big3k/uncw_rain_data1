
# compile reprojector, to show data on 0.1x0.1 grid
gfortran -O2 -g -mcmodel=medium -pipe -Wp,-D_FORTIFY_SOURCE=2 -Wp,-D_GLIBCXX_ASSERTIONS -fexceptions -fstack-protector-strong -grecord-gcc-switches -specs=/usr/lib/rpm/redhat/redhat-hardened-cc1 -specs=/usr/lib/rpm/redhat/redhat-annobin-cc1 -m64 -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection -I/usr/lib64/gfortran/modules  -I/usr/include -L/usr/lib64 -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -lnetcdff -Wl,-z,relro -Wl,-z,now -specs=/usr/lib/rpm/redhat/redhat-hardened-ld -fPIC -Wl,-z,now -Wl,--as-needed -lsz -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib64 \
-o reproj_data ../reproj_data.f90 ../check.f90

./reproj_data /tmp/20120302/NPP_ATMS_d20120302_t0834340_e1017269.nc NPP_ATMS_d20120302_t0834340_e1017269.bin

