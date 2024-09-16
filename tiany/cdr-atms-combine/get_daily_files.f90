

! Reading file list from given directory  
! Usage: 
!
! $0 daily_dir_gatmo daily_dir_tatms output_dir
! 
! where each directory contains the ~2700 matching files, such as: 
!
! GATMO_npp_d20120302_t2358446_e2359163_b01796_c20191106111734857346_ADu_dev.h5
!
! and
!
! TATMS_npp_d20120302_t2358446_e2359163_b01796_c20191106111734873108_ADu_dev.h5
!
! respectively. 
!
! Example: 
! $0 /data1/youy/cdr-atms-combine/gatmo/20120302 /data1/youy/cdr-atms-combine/tatms/20120302 /myfile/20120302

      program test_rd

      implicit none
      INCLUDE "parms.h"

      integer :: ndir, nf, jf, ibin, iargc, iret, nf_in_bin
      character(len=1064) :: gatmo_files(MAX_FILES_PER_DAY)       
      character(len=1064) :: tatms_files(MAX_FILES_PER_DAY)       
      character(len=1064) :: gatmo_files_in_bin(MAX_FILES_PER_DAY)       
      character(len=1064) :: tatms_files_in_bin(MAX_FILES_PER_DAY)       
      character(len=1064) :: tatms_matched_gatmo_file 
      character(len=1064) :: gatmo_dir 
      character(len=1064) :: tatms_dir
      character(len=1064) :: output_dir
      character(len=6) :: stime  ! start time of each file as string 
      integer :: nstime  ! start time of each file as integer 

      ndir =  iargc()
      If (ndir.ne.3) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"get_daiy_files daily_dir_gatmo daily_dir_tatms output_dir" 
         stop
      End If
      call getarg(1, gatmo_dir) 
      call getarg(2, tatms_dir) 
      call getarg(3, output_dir) 

      ! get the files
      call system("ls "//trim(gatmo_dir)// " | sort -n > /tmp/gatmofiles.txt")
      call system("ls "//trim(tatms_dir)// " | sort -n > /tmp/tatmsfiles.txt")
 
      ! read in gatmo file list ----------------------------
      open(31,FILE="/tmp/gatmofiles.txt",action="read")
      nf = 1
      do
         read(31,FMT='(a)',iostat=iret) gatmo_files(nf)
         if (iret /= 0) EXIT
         !write(*, *) trim(gatmo_files(nf))
         nf=nf+1 
      end do
      write(*, *)  "Number of gatmo files read: ", nf-1 
      close(31) 
      call system("rm /tmp/gatmofiles.txt")

      ! read in tatmos file list ----------------------------
      open(33,FILE="/tmp/tatmsfiles.txt",action="read")
      nf = 1
      do
         read(33,FMT='(a)',iostat=iret) tatms_files(nf)
         if (iret /= 0) EXIT
         !write(*, *) trim(tatms_files(nf))
         nf=nf+1
      end do
      write(*, *)  "Number of tatms files read: ", nf-1
      close(33) 
      call system("rm /tmp/tatmsfiles.txt")

! go over each time bin 
!==================================================================
      Do ibin=1, DAILY_BINS  !  
       !write(*, *) time_bins(ibin) 

       ! find all the files in the time bin 
       !------------------------------------------------
       nf_in_bin=0
       Do jf=1, nf-1 
         ! for each given gatmo file, find the matching tatms file 
         call match_gatmo_tatms_file(tatms_matched_gatmo_file, &
               gatmo_files(jf), tatms_files, nf-1) 
         if ( tatms_matched_gatmo_file .ne. "" ) then 
                ! found matching pairs file 
         stime=gatmo_files(jf)(22:27)
         read(stime, *) nstime
          if (nstime .ge.  time_bins(ibin) .and. nstime .lt.  &
               time_bins(ibin+1) ) then  
            nf_in_bin=nf_in_bin+1 
            gatmo_files_in_bin(nf_in_bin)=gatmo_files(jf)
            tatms_files_in_bin(nf_in_bin)=tatms_matched_gatmo_file
          end if 
         !write(*, *) nstime, stime, "  ", trim(gatmo_files(jf))
         !write(*, *) nstime, stime, "  ", trim(tatms_matched_gatmo_file)
         !write(*, *) "-------------------------------------------------"
        else 
         !write(*, *) nstime, stime, "  ", trim(gatmo_files(jf))
         !write(*, *) nstime, stime, "  "
         !write(*, *) "---------No Matching tatms file found------------"
        end if 
        
       End Do 
       !------------------------------------------------
       write(*, *) "for time bin=", ibin, " there are files: ", nf_in_bin
       ! Combine files for each bin 
       call combine_files(gatmo_files_in_bin, tatms_files_in_bin, &
            nf_in_bin, gatmo_dir, tatms_dir, output_dir) 

      End Do  
!==================================================================

      end 




