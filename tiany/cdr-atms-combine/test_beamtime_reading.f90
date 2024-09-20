
! Similar to merge_data(), but special treatment for
! "/All_Data/ATMS-TDR_All/BeamTime" and "Ascending/Descending_Indicator" 
! as the integer*8 former needs to be offsetted to fit into netcdf's
! NF90_INT. The new value in netcdf is seconds from 2020/1/1, while in
! the input hdf5 it is microseconds from 1958/1/1. 
! The latter needs to be read as an attribute, and inflate into a 2D
! array of the same dimension as beamtime.  

      program  test_merge_beamtime 

      implicit none
      INCLUDE "parms.h"
 
      integer :: ix, iy, iz, niy
      integer :: nx, ny, nz, jf, ibin, nf_in_bin
      integer :: nxt, nyt, nzt
      character(len=1064)    :: h5_dir, h5_file 
      character(len=1064) :: h5_files_in_bin(MAX_FILES_PER_DAY)
      character(len=1064)    :: var_name, group_name, att_name
      integer (kind=4)   :: time_buff(MAX_NX, MAX_NY, MAX_NZ*200)    
      integer (kind=4)   :: adflag_buff(MAX_NX, MAX_NY, MAX_NZ*200)    
      integer (kind=8)   :: h5_data(MAX_NX, MAX_NY, MAX_NZ)
      integer (kind=4)   :: h5_att(MAX_NX, MAX_NY, MAX_NZ), att_data
      real (kind=8), parameter   :: microsec = 1000000.0! microsec in sec 
      integer (kind=4), parameter   :: time_offset = 1325376000 !sec 
               ! when in 3D: (22, 96, 12) -> nchannel, nfov, nscan
               ! when in 2D: (96, 12) -> nfov, nscan
  
      h5_att=0 ! space to hold attributes in 2D
      var_name="/All_Data/ATMS-TDR_All/BeamTime"
      group_name="/Data_Products/ATMS-TDR/ATMS-TDR_Gran_0"
      att_name="Ascending/Descending_Indicator"
      h5_dir="/data1/youy/cdr-atms-combine/tatms/20120302"
      h5_file="TATMS_npp_d20120302_t2355326_e2356043_b01796_c20191106111645087805_ADu_dev.h5" 

! Calculation and verification of time offset 
![tiany@rain cdr-atms-combine]$ date -u -d "1958/1/1" +%s
!-378691200
![tiany@rain cdr-atms-combine]$ date -u -d "2000/1/1" +%s
!946684800
![tiany@rain cdr-atms-combine]$ calc 946684800+378691200
!1325376000
![tiany@rain cdr-atms-combine]$ date -u -d "2012/3/2"
!Fri Mar  2 00:00:00 UTC 2012
![tiany@rain cdr-atms-combine]$ date -u -d "2012/3/2 +%s"
!date: invalid date ‘2012/3/2 +%s’
![tiany@rain cdr-atms-combine]$ date -u -d "2012/3/2" +%s
!1330646400
![tiany@rain cdr-atms-combine]$ calc 1330646400-1325376000
!5270400
![tiany@rain cdr-atms-combine]$ date -u -d "2020/1/1 +5270400 sec"
!Mon Mar  2 00:00:00 UTC 2020


        call read_h5_var_bigint(trim(h5_dir)//"/"//trim(h5_file), &
                         var_name, h5_data, nx, ny, nz) 
        write(*, *)"nx=", nx, " ny=", ny, " nz=", nz

           Do iy=1, ny
            niy=niy+1 
            Do ix=1, nx 
             time_buff(ix, niy, 1) = nint(h5_data(ix, iy, 1)/microsec) & 
                                     -time_offset  
             write(*, *) "h5_data=", h5_data(ix, iy, 1), "time_buff=", &
                            time_buff(ix, iy, 1)  
            End Do
           End Do 

       
      return 

      end 
       
      


