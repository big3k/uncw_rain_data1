
#gfortran -cpp -O2 -shared-libgcc -fdec-math -fdec-structure  -DLANGUAGE_FORTRAN  -DWRITEPIAINFO -c fov_dim.f90

h5fc -cpp -O2 -fdec-math -fdec-structure -DLANGUAGE_FORTRAN  -DWRITEPIAINFO -o reproj reproj.F90 fov_dim.f90 fov_orc.f90 fov2gc.f90

#h5fc -cpp -DLANGUAGE_FORTRAN  -DWRITEPIAINFO -o reproj reproj.F90 fov_dim.f90 fov_orc.f90 fov2gc.f90

exit

 gfortran -cpp -O2 -shared-libgcc -fdec-math -fdec-structure  -DLANGUAGE_FORTRAN  -DWRITEPIAINFO -o test_fov test_fov.F90 fov_dim.f90

exit

h5fc -o reproj reproj.F90



#h5fc -o test_h5 test_h5.F90 


#[tiany@rain Regrid]$ h5fc -show
#gfortran -O2 -g -pipe -Wall -Werror=format-security -Wp,-D_FORTIFY_SOURCE=2 -Wp,-D_GLIBCXX_ASSERTIONS -fexceptions -fstack-protector-strong -grecord-gcc-switches -specs=/usr/lib/rpm/redhat/redhat-hardened-cc1 -specs=/usr/lib/rpm/redhat/redhat-annobin-cc1 -m64 -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection -I/usr/lib64/gfortran/modules -I/usr/include -L/usr/lib64 -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -Wl,-z,relro -Wl,-z,now -specs=/usr/lib/rpm/redhat/redhat-hardened-ld -fPIC -Wl,-z,now -Wl,--as-needed -lsz -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib64

