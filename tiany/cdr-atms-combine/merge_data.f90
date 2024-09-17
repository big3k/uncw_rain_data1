
! Given a varable, put its values from the list of h5 files in data_buff 

      subroutine  merge_data(var_name, h5_dir, h5_files_in_bin, & 
                        nf_in_bin, data_buff, nx, ny, nz) 

      implicit none
      INCLUDE "parms.h"
 
      integer :: ix, iy, iz, niy
      integer :: nx, ny, nz, jf, ibin, nf_in_bin
      integer :: nxt, nyt, nzt
      character(len=*)    :: h5_dir 
      character(len=1064) :: h5_files_in_bin(MAX_FILES_PER_DAY)
      character(len=*)    :: var_name 
      real (kind=4)   :: data_buff(MAX_NX, MAX_NY, MAX_NZ*200)    
      real (kind=4)   :: h5_data(MAX_NX, MAX_NY, MAX_NZ), scale_data(2)
      real (kind=4)   :: tscale, toff 
               ! when in 3D: (22, 96, 12) -> nchannel, nfov, nscan
               ! when in 2D: (96, 12) -> nfov, nscan

      niy=0
      Do jf=1, nf_in_bin
        call read_h5_var(trim(h5_dir)//"/"//trim(h5_files_in_bin(jf)), &
                         var_name, h5_data, nx, ny, nz) 
        if ( nz .eq. 1 ) then  ! 2D data, such as longitude
           ! accumulate in ny direction
           Do iy=1, ny
            niy=niy+1 
            Do ix=1, nx 
             data_buff(ix, niy, 1) = h5_data(ix, iy, 1)  
            End Do
           End Do 
        else if ( nz .gt. 1 ) then ! 3D data, accumulate in nz direction
           tscale=1
           toff=0
           if ( trim(var_name) .eq.  &
                "/All_Data/ATMS-TDR_All/AntennaTemperature") then 
                ! read in scale fators 
                call read_h5_var(trim(h5_dir)//"/"//trim(h5_files_in_bin(jf)), &
                    "/All_Data/ATMS-TDR_All/AntennaTemperatureFactors", &
                    scale_data, nxt, nyt, nzt) 
                tscale=scale_data(1) 
                toff=scale_data(2) 
           end if 
                
           Do iz=1, nz
            niy=niy+1 
            Do iy=1, ny
              Do ix=1, nx 
               data_buff(ix, iy, niy) = h5_data(ix, iy, iz)*tscale+toff  
              End Do
            End Do 
           End Do 
        else 
           write(*, *) "Data dimensions are not supported." 
           stop
        end if 
           
      End Do ! all files gone over 
      if ( nz .eq. 1 ) then  ! 2D data, such as longitude
        ny = niy  ! merged nscans
      else ! 3D data 
        nz = niy
      end if 
       
      return 

      end 
       
      


