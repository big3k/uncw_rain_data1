
! Combine a list of h5 files and save to netcdf 

      subroutine  combine_files(gatmo_files_in_bin, tatms_files_in_bin, &
            nf_in_bin, gatmo_dir, tatms_dir, output_dir)

      use netcdf 

      implicit none
      INCLUDE "parms.h"
 
      integer :: ndir, nf, jf, ibin, nf_in_bin, nx, ny, nz, ng
      integer :: ix, iy, iz , oldmode
      character(len=1064) :: gatmo_files_in_bin(MAX_FILES_PER_DAY)
      character(len=1064) :: tatms_files_in_bin(MAX_FILES_PER_DAY)
      character(len=1064) :: gatmo_dir
      character(len=1064) :: tatms_dir
      character(len=1064) :: output_dir
      character(len=1064) :: outf 
      real*4 :: data_buff(MAX_NX, MAX_NY, MAX_NZ*200)    
      integer (kind=8)   :: time_buff(MAX_NX, MAX_NY, MAX_NZ*200)
      integer (kind=4)   :: adflag_buff(MAX_NX, MAX_NY, MAX_NZ*200)

      real*4, allocatable :: d3data(:, :, :), d2data(:, :) 
      integer*4, allocatable :: id2data(:, :) 
      integer*8, allocatable :: id2data64(:, :) 
      integer :: ncid, status
      integer :: d2dims(2), d3dims(3), d3dims_bm(3) 
      INTEGER(KIND=4) :: x_dimid, y_dimid, z_dimid, g_dimid  
                         ! g_dimid is group for BeamLatitude/BeamLongitude
      INTEGER(KIND=4) :: at_varid, lon_varid, lat_varid, sza_varid 
      INTEGER(KIND=4) :: sol_varid, bmlat_varid, bmlon_varid  
      INTEGER(KIND=4) :: bt_varid, adi_varid

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
      ! 10/4/2024: critcal: open it as NF90_NETCDF4, so we can use
      ! NF90_INT64 data types. 
      !status=nf90_create(trim(output_dir)//"/"//trim(outf), NF90_CLOBBER, ncid)
      call check(nf90_create(trim(output_dir)//"/"//trim(outf), NF90_NETCDF4, ncid))
      !call check(nf90_set_fill(ncid, nf90_nofill, oldmode))
     
      !get 3D data first, so we can define the dimensions 
      call merge_data("/All_Data/ATMS-TDR_All/AntennaTemperature", tatms_dir, & 
            tatms_files_in_bin, nf_in_bin, data_buff, nx, ny, nz)  
      ! scale to be added
      
      !Define the dimensions.
      ng=5 ! hardcoded for BeamLatitude/BeamLongitude. 
      call check(nf90_def_dim(ncid, "nchannel", nx, x_dimid))
      call check(nf90_def_dim(ncid, "nfovs", ny, y_dimid))
      call check(nf90_def_dim(ncid, "nscans", nz, z_dimid))
      call check(nf90_def_dim(ncid, "ngroups", ng, g_dimid))
      d3dims=(/ x_dimid, y_dimid, z_dimid /)
      d2dims=(/ y_dimid, z_dimid /)
      d3dims_bm=(/ g_dimid, y_dimid, z_dimid /) ! for BeamLatitude/BeamLongitude.

      ! Define all the variables 
      call check(nf90_def_var(ncid, "AntennaTemperature", &
             NF90_FLOAT, d3dims, at_varid, deflate_level=6))
      status=nf90_def_var(ncid, "Longitude", &
             NF90_FLOAT, d2dims, lon_varid, deflate_level=6)
      status=nf90_def_var(ncid, "Latitude", &
             NF90_FLOAT, d2dims, lat_varid, deflate_level=6)
      status=nf90_def_var(ncid, "SatelliteZenithAngle", &
             NF90_FLOAT, d2dims, sza_varid, deflate_level=6)
      call check(nf90_def_var(ncid, "SolarZenithAngle", &
             NF90_FLOAT, d2dims, sol_varid, deflate_level=6))
      call check(nf90_def_var(ncid, "BeamLatitude", &
             NF90_FLOAT, d3dims_bm, bmlat_varid, deflate_level=6))
      call check(nf90_def_var(ncid, "BeamLongitude", &
             NF90_FLOAT, d3dims_bm, bmlon_varid, deflate_level=6))
      call check(nf90_def_var(ncid, "BeamTime", &
             NF90_INT64, d2dims, bt_varid, deflate_level=6))
             !NF90_INT, d2dims, bt_varid, deflate_level=6)
      call check(nf90_def_var(ncid, "Ascending_Descending_Indicator", &
             NF90_INT, d2dims, adi_varid, deflate_level=6))
      call check(nf90_enddef(ncid))

      allocate(d3data(nx, ny, nz)) 
      d3data = data_buff(1:nx, 1:ny, 1:nz) 
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
      status=nf90_put_var(ncid, lat_varid, d2data)
      deallocate(d2data)

     !get 2D data
      call merge_data("/All_Data/ATMS-SDR-GEO_All/SatelliteZenithAngle", gatmo_dir, &
            gatmo_files_in_bin, nf_in_bin, data_buff, nx, ny, nz)

      allocate(d2data(nx, ny))
      d2data = data_buff(1:nx, 1:ny, 1)
      status=nf90_put_var(ncid, sza_varid, d2data)
      deallocate(d2data)

     !get 2D data
      call merge_data("/All_Data/ATMS-SDR-GEO_All/SolarZenithAngle", gatmo_dir, &
            gatmo_files_in_bin, nf_in_bin, data_buff, nx, ny, nz)

      allocate(d2data(nx, ny))
      d2data = data_buff(1:nx, 1:ny, 1)
      status=nf90_put_var(ncid, sol_varid, d2data)
      deallocate(d2data)

      !get 3D data 
      call merge_data("/All_Data/ATMS-SDR-GEO_All/BeamLatitude", gatmo_dir, & 
            gatmo_files_in_bin, nf_in_bin, data_buff, ng, ny, nz)  
      !double check ng=5
      if ( ng .ne. 5) then 
         write(*, *)"ng!=5 ... something is wrong" 
         stop
      end if
      allocate(d3data(ng, ny, nz)) 
      d3data = data_buff(1:ng, 1:ny, 1:nz) 
      status=nf90_put_var(ncid, bmlat_varid, d3data)
      deallocate(d3data) 

      !get 3D data
      call merge_data("/All_Data/ATMS-SDR-GEO_All/BeamLongitude", gatmo_dir, &
            gatmo_files_in_bin, nf_in_bin, data_buff, ng, ny, nz)
      !double check ng=5
      if ( ng .ne. 5) then
         write(*, *)"ng!=5 ... something is wrong"
         stop
      end if
      allocate(d3data(ng, ny, nz))
      d3data = data_buff(1:ng, 1:ny, 1:nz)
      status=nf90_put_var(ncid, bmlon_varid, d3data)
      deallocate(d3data)

      ! special handliing of BeamTime and A/Descending Indicator 
      call merge_beamtime_adflag_data(tatms_dir, tatms_files_in_bin, &
                        nf_in_bin, time_buff, adflag_buff, nx, ny, nz)
      allocate(id2data64(nx, ny))
      id2data64 = time_buff(1:nx, 1:ny, 1)
      status=nf90_put_var(ncid, bt_varid, id2data64)
      deallocate(id2data64)
      allocate(id2data(nx, ny))
      id2data = adflag_buff(1:nx, 1:ny, 1)
      status=nf90_put_var(ncid, adi_varid, id2data)
      deallocate(id2data)
       
      status=nf90_close(ncid)
      return 

      end 
       
        
      


