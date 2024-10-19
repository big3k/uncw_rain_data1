#! /bin/bash

gfortran -O2 -g -mcmodel=medium -pipe -Wp,-D_FORTIFY_SOURCE=2 -Wp,-D_GLIBCXX_ASSERTIONS -fexceptions -fstack-protector-strong -grecord-gcc-switches -m64 -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection -I/usr/lib64/gfortran/modules  -I/usr/include -L/usr/lib64 -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -lnetcdff -Wl,-z,relro -Wl,-z,now -fPIC -Wl,-z,now -Wl,--as-needed -lsz -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib64 \
  -o merge_npp_atms_daily get_orbit_files.f90 combine_files.f90 merge_data.f90 merge_beamtime_adflag_data.f90 read_h5_var.f90 read_h5_var_bigint.f90 read_h5_att.f90 check.f90



