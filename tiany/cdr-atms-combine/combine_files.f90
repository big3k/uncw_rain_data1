
! Combine a list of h5 files and save to netcdf 

      subroutine  combine_files(gatmo_files_in_bin, tatms_files_in_bin, &
            nf_in_bin, gatmo_dir, tatms_dir, output_dir)

      implicit none
      INCLUDE "parms.h"
 
      integer :: ndir, nf, jf, ibin, nf_in_bin
      character(len=1064) :: gatmo_files_in_bin(MAX_FILES_PER_DAY)
      character(len=1064) :: tatms_files_in_bin(MAX_FILES_PER_DAY)
      character(len=1064) :: gatmo_dir
      character(len=1064) :: tatms_dir
      character(len=1064) :: output_dir
      character(len=1064) :: outf 

      character(len=6) :: stime  ! start time string of combined file 
      character(len=6) :: etime  ! end time string of combined file 
 
      stime=gatmo_files_in_bin(1)(21:28) 
      etime=gatmo_files_in_bin(nf_in_bin)(30:37) 
      ! construct output file name
      ! e.g., COMBO_npp_d20120302_t2034286_e2217243_b01794_c20191106092131140068_ADu_dev.nc
      outf=gatmo_files_in_bin(1)
      outf(30:37)=gatmo_files_in_bin(nf_in_bin)(30:37)
      outf(1:5)="COMBO"
      outf(76:77)="nc"
      write(*, *) trim(outf) 
      ! read every file in bin
      Do jf=1, nf_in_bin
       call read_h5_var(trim(gatmo_dir)//"/"//trim(gatmo_files_in_bin(jf)), &
                   "/All_Data/ATMS-SDR-GEO_All/Longitude")  
       call read_h5_var(trim(tatms_dir)//"/"//trim(tatms_files_in_bin(jf)), &
                   "/All_Data/ATMS-SDR-GEO_All/Longitude")  
      
       ! stuff to big arrary
        
      End Do
       
      return 

      end 
       
        
      


