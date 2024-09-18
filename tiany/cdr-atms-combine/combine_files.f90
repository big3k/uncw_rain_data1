
! Combine a list of h5 files and save to netcdf 

      subroutine  combine_files(gatmo_files_in_bin, tatms_files_in_bin, &
            nf_in_bin, gatmo_dir, tatms_dir, output_dir)

      use netcdf 

      implicit none
      INCLUDE "parms.h"
 
      integer :: ndir, nf, jf, ibin, nf_in_bin, nx, ny, nz
      integer :: ix, iy, iz 
      character(len=1064) :: gatmo_files_in_bin(MAX_FILES_PER_DAY)
      character(len=1064) :: tatms_files_in_bin(MAX_FILES_PER_DAY)
      character(len=1064) :: gatmo_dir
      character(len=1064) :: tatms_dir
      character(len=1064) :: output_dir
      character(len=1064) :: outf 
      real*4 :: data_buff(MAX_NX, MAX_NY, MAX_NZ*200)    
      real*4, allocatable :: d3data(:, :, :), d2data(:, :) 
      integer :: ncid, status
      integer :: d2dims(2), d3dims(3) 
      INTEGER(KIND=4) :: x_dimid, y_dimid, z_dimid
      INTEGER(KIND=4) :: at_varid, lon_varid, lat_varid, sza_varid 

      character(len=9) :: dtime  ! d20120302
      character(len=8) :: stime  ! start time string of combined file, t2034286 
      character(len=8) :: etime  ! end time string of combined file , e2217243
 
      dtime=gatmo_files_in_bin(1)(11:19) 
      stime=gatmo_files_in_bin(1)(21:28) 
      etime=gatmo_files_in_bin(nf_in_bin)(30:37) 
      ! construct output file name
      ! e.g., COMBO_npp_d20120302_t2034286_e2217243_b01794_c20191106092131140068_ADu_dev.nc
      ! NPP_ATMS_d20120302_t1708393_e1851323.nc
      outf="NPP_ATMS_"//dtime//"_"//stime//"_"//etime//".nc" 
      write(*, *) trim(outf) 
      
      !open netcdf for saving variables 
      status=nf90_create(trim(output_dir)//"/"//trim(outf), NF90_CLOBBER, ncid)
     
      !get 3D data first, so we can define the dimensions 
      call merge_data("/All_Data/ATMS-TDR_All/AntennaTemperature", tatms_dir, & 
            tatms_files_in_bin, nf_in_bin, data_buff, nx, ny, nz)  
      ! scale to be added
      
      !Define the dimensions.
      status=nf90_def_dim(ncid, "nchannel", nx, x_dimid)
      status=nf90_def_dim(ncid, "nfovs", ny, y_dimid)
      status=nf90_def_dim(ncid, "nscans", nz, z_dimid)
      d3dims=(/ x_dimid, y_dimid, z_dimid /)
      d2dims=(/ y_dimid, z_dimid /)

      ! Define all the variables 
      status=nf90_def_var(ncid, "AntennaTemperature", &
             NF90_FLOAT, d3dims, at_varid, deflate_level=6)
      status=nf90_def_var(ncid, "Longitude", &
             NF90_FLOAT, d2dims, lon_varid, deflate_level=6)
      status=nf90_def_var(ncid, "Latitude", &
             NF90_FLOAT, d2dims, lat_varid, deflate_level=6)
      status=nf90_def_var(ncid, "SatelliteZenithAngle", &
             NF90_FLOAT, d2dims, sza_varid, deflate_level=6)
      status=nf90_enddef(ncid)

      allocate(d3data(nx, ny, nz)) 
      d3data = data_buff(1:nx, 1:ny, 1:nz) 
      !status=nf90_def_var(ncid, "/All_Data/ATMS-TDR_All/AntennaTemperature", &

      !status=nf90_enddef(ncid)
      status=nf90_put_var(ncid, at_varid, d3data)
      deallocate(d3data) 

      !get 2D data 
      call merge_data("/All_Data/ATMS-SDR-GEO_All/Longitude", gatmo_dir, &
            gatmo_files_in_bin, nf_in_bin, data_buff, nx, ny, nz)
      ! scale to be added

      allocate(d2data(nx, ny))
      d2data = data_buff(1:nx, 1:ny, 1)

      status=nf90_put_var(ncid, lon_varid, d2data)
      deallocate(d2data)

     !get 2D data
      call merge_data("/All_Data/ATMS-SDR-GEO_All/Latitude", gatmo_dir, &
            gatmo_files_in_bin, nf_in_bin, data_buff, nx, ny, nz)
      ! scale to be added

      allocate(d2data(nx, ny))
      d2data = data_buff(1:nx, 1:ny, 1)

      !status=nf90_enddef(ncid)
      status=nf90_put_var(ncid, lat_varid, d2data)
      deallocate(d2data)

     !get 2D data
      call merge_data("/All_Data/ATMS-SDR-GEO_All/SatelliteZenithAngle", gatmo_dir, &
            gatmo_files_in_bin, nf_in_bin, data_buff, nx, ny, nz)
      ! scale to be added

      allocate(d2data(nx, ny))
      d2data = data_buff(1:nx, 1:ny, 1)

      status=nf90_put_var(ncid, sza_varid, d2data)
      deallocate(d2data)
100   continue
 
      ! Get "/All_Data/ATMS-TDR_All/BeamTime" from TATMS files
      ! However, it has "H5T_STD_I64LE" type, which won't fit in netcdf
      !          DATASET "BeamTime" {
      !      DATATYPE  H5T_STD_I64LE
      !      DATASPACE  SIMPLE { ( 12, 96 ) / ( H5S_UNLIMITED, H5S_UNLIMITED ) }

      !Note that there are no netCDF types corresponding to 64-bit integers or 
      ! to characters wider than 8 bits in the current version of the netCDF library.
       
      status=nf90_close(ncid)
      return 

      end 
       
        
      


