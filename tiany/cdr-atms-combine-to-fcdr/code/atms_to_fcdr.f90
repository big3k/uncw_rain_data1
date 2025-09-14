!
! Read an atms nc file and writes to fcdr-style nc. 
! 9/14/2025 

      program atmos2fcdr

      use netcdf 
      use eswath_mod
      use set_sw_mod

      implicit none
      INCLUDE "parms.h"
 
! NC file
!dimensions:
!        nchannel = 22 ;
!        nfovs = 96 ;
!        nscans = 2328 ;

      integer  :: nchannel,  nfovs, nscans 
      ! 0.1-deg
      real, parameter :: lat0=-89.95, lon0=-179.95, res=0.1
      integer, parameter :: nc=3600, nr=1800  ! lat/lon grid
      ! 0.25-deg
      !real, parameter :: lat0=-89.875, lon0=-179.875, res=0.25
      !integer, parameter :: nc=1440, nr=720  ! lat/lon grid

      integer :: ndir, nf, jf, ibin, nf_in_bin, nx, ny, nz, iargc
      integer :: ic, ir, iz , i, j
      character(len=1064) :: nc_atms
      character(len=1064) :: nc_outf 
      character(len=1064) :: nchannel_name, nfovs_name, nscans_name 
      ! NC data buffer
      real*4, allocatable :: grid_bt(:, :, :) 

      real*4, allocatable :: bt_data(:, :, :), lon(:, :), lat(:, :) 
      integer(kind=8), allocatable :: BeamTime(:,:)
      integer*4, allocatable :: id2data(:, :) 
      integer :: ncid, status, nrec
      integer :: d2dims(2), d3dims(3) 
      INTEGER(KIND=4) :: x_dimid, y_dimid, z_dimid
      INTEGER(KIND=4) :: at_varid, lon_varid, lat_varid, sza_varid 
      INTEGER(KIND=4) :: bt_varid, adi_varid, btime_varid

      ndir =  iargc()
      If (ndir.ne.2) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"atms_to_fcdr input_nc_file output_fcdr_file" 
         stop
      End If
      call getarg(1, nc_atms)
      call getarg(2, nc_outf) 
 
      !call set_sw(nc_outf, 1000) ! testing numscan=1000
      !stop
      
      !open netcdf for reading atms variables 
      call check(nf90_open(nc_atms, nf90_nowrite, ncid))
      call check(nf90_inquire_dimension(ncid,1, nchannel_name, nchannel) )
      call check(nf90_inquire_dimension(ncid,2, nfovs_name, nfovs) )
      call check(nf90_inquire_dimension(ncid,3, nscans_name, nscans) )

      write(*, *) "nchannel=", nchannel, " nfovs=", nfovs, " nscans=", nscans 

      allocate(bt_data(nchannel, nfovs, nscans) )
      allocate(lat(nfovs, nscans) )
      allocate(lon(nfovs, nscans) )
      allocate(BeamTime(nfovs, nscans) )

      call check(nf90_inq_varid(ncid, "AntennaTemperature", at_varid) )
      call check(nf90_get_var(ncid,at_varid,bt_data))
      call check(nf90_inq_varid(ncid, "Longitude", lon_varid) )
      call check(nf90_get_var(ncid,lon_varid,lon))
      call check(nf90_inq_varid(ncid, "Latitude", lat_varid) )
      call check(nf90_get_var(ncid,lat_varid,lat))
      call check(nf90_inq_varid(ncid, "BeamTime", btime_varid) )
      call check(nf90_get_var(ncid,btime_varid,BeamTime))

      call check(nf90_close(ncid)) 

      write(*, *) "BeamTime(1, 1)=", BeamTime(1, 1) 

      ! stuff fcdr data structure 
      lat_a1_1(1:nscans, 1:nfovs) = lat
      lon_a1_1(1:nscans, 1:nfovs) = lon
      lat_a1_2(1:nscans, 1:nfovs) = lat
      lon_a1_2(1:nscans, 1:nfovs) = lon
      lat_a2(1:nscans, 1:nfovs) = lat
      lon_a2(1:nscans, 1:nfovs) = lon
      
      call set_sw(nc_outf, nscans) ! testing 
      return 

      end 
       
        
      


