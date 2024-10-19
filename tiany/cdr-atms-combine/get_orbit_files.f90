
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

      program merge_npp_atms_daily

      implicit none
      INCLUDE "parms.h"

      integer :: ndir, nf, jf, ibin, iargc, iret, nf_in_bin
      character(len=1064) :: gatmo_files_in_bin(MAX_FILES_PER_DAY)       
      character(len=1064) :: tatms_files_in_bin(MAX_FILES_PER_DAY)       
      character(len=1064) :: gatmo_dir, gatmo_files_list  
      character(len=1064) :: tatms_dir, tatms_files_list 
      character(len=1064) :: output_file 

      ndir =  iargc()
      If (ndir.ne.3) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"merge_npp_atms_daily gatmo_files_list tatms_files_list output_filename" 
         stop
      End If
      call getarg(1, gatmo_files_list) 
      call getarg(2, tatms_files_list) 
      call getarg(3, output_file) 
      !"
 
      ! read in gatmo file list ----------------------------
      open(31,FILE=trim(gatmo_files_list),action="read")
      nf = 1
      do
         read(31,FMT='(a)',iostat=iret) gatmo_files_in_bin(nf)
         if (iret /= 0) EXIT
         !write(*, *) trim(gatmo_files(nf))
         nf=nf+1 
      end do
      !write(*, *)  "Number of gatmo files read for the orbit: ", nf-1 
      close(31) 

      ! read in tatmos file list ----------------------------
      open(33,FILE=trim(tatms_files_list),action="read")
      nf = 1
      do
         read(33,FMT='(a)',iostat=iret) tatms_files_in_bin(nf)
         if (iret /= 0) EXIT
         !write(*, *) trim(tatms_files(nf))
         nf=nf+1
      end do
      !write(*, *)  "Number of tatms files read: ", nf-1
      close(33) 

      nf_in_bin=nf-1
      ! Combine files for each orbit 
      gatmo_dir=""  ! legacy, fill with empty. Each file now in tatms_files_in_bin 
      tatms_dir=""  ! and gatmo_files_in_bin has full path
      call combine_files(gatmo_files_in_bin, tatms_files_in_bin, &
           nf_in_bin, gatmo_dir, tatms_dir, output_file) 


      end 
