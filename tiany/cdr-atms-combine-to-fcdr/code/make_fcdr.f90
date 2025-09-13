
! Get gatmo and tatms file lists and output filename from cmd line 
! Usage: 
!
! $0 gatmo_list_file tatms_list_file output_file 
! 
! where each list_file contains the list of h5 files to be combined into
! the output file, such as: 
!
! GATMO_npp_d20120302_t2358446_e2359163_b01796_c20191106111734857346_ADu_dev.h5
! and
! TATMS_npp_d20120302_t2358446_e2359163_b01796_c20191106111734873108_ADu_dev.h5
!
! respectively. 
!
! Example: 
! $0 /tmp/tiany/gatmo_b01782.txt /tmp/tiany/tatms_b01782.txt /tmp/2012/20120302/NPP_ATMS_d20120302_t0000286_e0048283_b001782.nc

      program make_fcdr 

      use eswath_mod
      implicit none

      INCLUDE "parms.h"

      integer :: ndir, nf, jf, ibin, iargc, iret, nf_in_bin
      character(len=1064) :: gatmo_files_in_bin(MAX_FILES_PER_DAY)       
      character(len=1064) :: tatms_files_in_bin(MAX_FILES_PER_DAY)       
      character(len=1064) :: gatmo_dir, gatmo_files_list  
      character(len=1064) :: tatms_dir, tatms_files_list 
      character(len=1064) :: output_file 

      output_file="test.nc" 
      call set_sw(output_file, 200) 
      
      end 
