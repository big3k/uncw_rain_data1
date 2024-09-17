
h5fc -cpp -O2 -fdec-math -fdec-structure -DLANGUAGE_FORTRAN  -DWRITEPIAINFO -o test_read_h5_var test_read_h5_var.f90 read_h5_var.f90

exit 

gfortran -o get_daily_files get_daily_files.f90 match_gatmo_tatms_file.f90 combine_files.f90 read_h5_var.f90

./get_daily_files  /data1/youy/cdr-atms-combine/gatmo/20120302 /data1/youy/cdr-atms-combine/tatms/20120302 /tmp/20120302
