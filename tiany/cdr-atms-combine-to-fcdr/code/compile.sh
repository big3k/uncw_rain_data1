#! /bin/bash


#gfortran -c constants.f90
#gfortran -c geo_time.f90
#gfortran -c sys_time.f90 
#gfortran -c in_params.f90
#gfortran -c ama1b.f90
#gfortran -c eswath_mod.f90
#gfortran -fno-range-check -c set_sw.f90 -I/usr/lib64/gfortran/modules  -I/usr/include

gfortran  -fno-range-check -I/usr/lib64/gfortran/modules -I/usr/include -L/usr/lib64 \
  -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -lnetcdff -Wl,-z,relro -Wl,-z,now \
  -fPIC -Wl,-z,now -Wl,--as-needed -lsz -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib64 \
  -o atms_to_fcdr atms_to_fcdr.f90 set_sw.f90 constants.f90 geo_time.f90 sys_time.f90 in_params.f90 eswath_mod.f90

exit


gfortran -O2 -g -mcmodel=medium -pipe -Wp,-D_FORTIFY_SOURCE=2 -Wp,-D_GLIBCXX_ASSERTIONS -fexceptions -fstack-protector-strong -grecord-gcc-switches -m64 -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection -I/usr/lib64/gfortran/modules  -I/usr/include -L/usr/lib64 -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -lnetcdff -Wl,-z,relro -Wl,-z,now -fPIC -Wl,-z,now -Wl,--as-needed -lsz -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib64 \
-o print_BeamTime print_BeamTime.f90 check.f90  epoch_to_datetime.f90


