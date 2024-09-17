
      program test_rd


      implicit none 
      include "parms.h" 

      character(len=1064) :: h5_file, group_name, att_name, dir
      
      real (kind=4)   :: h5_data(MAX_NX, MAX_NY, MAX_NZ)
      integer :: nx, ny, nz, ix, iy, iz, att_data

      ! GATMO
      dir="/data1/youy/cdr-atms-combine/gatmo/20120302/"
      h5_file=trim(dir)// & 
         "GATMO_npp_d20120302_t2356046_e2356363_b01796_c20191106111652407644_ADu_dev.h5"

      ! TATMS
      dir="/data1/youy/cdr-atms-combine/tatms/20120302/"
      h5_file=trim(dir)// & 
         "TATMS_npp_d20120302_t2355326_e2356043_b01796_c20191106111645087805_ADu_dev.h5"

      !att_name="/All_Data/ATMS-SDR-GEO_All/Longitude" 
      ! in TATMS 
      !att_name="/All_Data/ATMS-TDR_All/AntennaTemperature"
      group_name="/Data_Products/ATMS-TDR/ATMS-TDR_Gran_0" 
      att_name="Ascending/Descending_Indicator"
      call read_h5_att(h5_file, group_name, att_name, att_data) 

      write(*, *) "att_data: ", att_data 

      end 


