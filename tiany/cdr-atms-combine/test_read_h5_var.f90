
      program test_rd


      implicit none 
      include "parms.h" 

      character(len=1064) :: h5_file, var_name, dir
      
      real (kind=4)   :: h5_data(MAX_NX, MAX_NY, MAX_NZ)
      integer :: nx, ny, nz, ix, iy, iz

      dir="/data1/youy/cdr-atms-combine/gatmo/20120302/"
      h5_file=trim(dir)// & 
         "GATMO_npp_d20120302_t2356046_e2356363_b01796_c20191106111652407644_ADu_dev.h5"

      var_name="/All_Data/ATMS-SDR-GEO_All/Longitude" 
      call read_h5_var(h5_file, var_name, h5_data, nx, ny, nz)

      write(*, *) "nx=", nx, " ny=", ny, " nz=", nz

      Do iz=1, nz
        Do iy=1, ny
         Do ix=1, nx 
          !write(*, *) ix, iy, h5_data(ix, iy, iz) 
          write(*, *) h5_data(ix, iy, iz) 
         End Do 
        End Do 
      End Do 

      end 


