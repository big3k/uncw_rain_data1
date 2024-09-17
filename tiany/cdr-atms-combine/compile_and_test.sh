
gfortran -O2 -g -pipe -Wall -Werror=format-security -Wp,-D_FORTIFY_SOURCE=2 -Wp,-D_GLIBCXX_ASSERTIONS -fexceptions -fstack-protector-strong -grecord-gcc-switches -specs=/usr/lib/rpm/redhat/redhat-hardened-cc1 -specs=/usr/lib/rpm/redhat/redhat-annobin-cc1 -m64 -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection -I/usr/lib64/gfortran/modules  -I/usr/include -L/usr/lib64 -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -lnetcdff -Wl,-z,relro -Wl,-z,now -specs=/usr/lib/rpm/redhat/redhat-hardened-ld -fPIC -Wl,-z,now -Wl,--as-needed -lsz -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib64 \
  -o get_daily_files get_daily_files.f90 match_gatmo_tatms_file.f90 combine_files.f90 merge_data.f90 read_h5_var.f90

./get_daily_files  /data1/youy/cdr-atms-combine/gatmo/20120302 /data1/youy/cdr-atms-combine/tatms/20120302 /tmp/20120302

#h5fc -cpp -O2 -fdec-math -fdec-structure -DLANGUAGE_FORTRAN  -DWRITEPIAINFO -o get_daily_files get_daily_files.f90 match_gatmo_tatms_file.f90 combine_files.f90 merge_data.f90 read_h5_var.f90

exit

exit 
h5fc -cpp -O2 -fdec-math -fdec-structure -DLANGUAGE_FORTRAN  -DWRITEPIAINFO -o test_read_h5_att test_read_h5_att.f90 read_h5_att.f90


h5fc -cpp -O2 -fdec-math -fdec-structure -DLANGUAGE_FORTRAN  -DWRITEPIAINFO -o test_read_h5_var test_read_h5_var.f90 read_h5_var.f90

exit 


