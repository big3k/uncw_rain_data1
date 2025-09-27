!
! Read an atms nc file and writes to fcdr-style nc. 
! 9/14/2025 

      program atmos2fcdr

      use iso_c_binding
      use netcdf 
      use constants
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
      integer :: ic, ir, iz , i, j, ich, ifov, iscan
      character(len=1064) :: nc_atms
      character(len=1064) :: nc_outf 
      character(len=1064) :: nchannel_name, nfovs_name, nscans_name 
      character(len=25) :: timestamp
      integer(c_int64_t) :: epoch

      ! NC data buffer
      real*4, allocatable :: grid_bt(:, :, :) 

      real*4, allocatable :: bt_data(:, :, :), lon(:, :), lat(:, :) 
      integer, allocatable :: AD_Indicator(:, :)
      real*4, allocatable ::  SatelliteZenithAngle(:, :), SolarZenithAngle(:, :) 
      integer(kind=8), allocatable :: BeamTime(:,:)
      integer*4, allocatable :: id2data(:, :) 
      integer :: ncid, status, nrec
      integer :: d2dims(2), d3dims(3) 
      INTEGER(KIND=4) :: x_dimid, y_dimid, z_dimid
      INTEGER(KIND=4) :: at_varid, lon_varid, lat_varid, sza_varid 
      INTEGER(KIND=4) :: bt_varid, adi_varid, btime_varid, ad_varid
      INTEGER(KIND=4) :: any_varid

      ndir =  iargc()
      If (ndir.ne.2) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"atms_to_fcdr input_nc_file output_fcdr_file" 
         stop
      End If
      call getarg(1, nc_atms)
      call getarg(2, nc_outf) 
      call get_basename(nc_atms, hdf_fname) ! hdf_fname will go to "source" attr in output netcdf
 
      !call set_sw(nc_outf, 1000) ! testing numscan=1000
      !stop
      
      !open netcdf for reading atms variables 
      call check(nf90_open(nc_atms, nf90_nowrite, ncid))
      call check(nf90_inquire_dimension(ncid,1, nchannel_name, nchannel) )
      call check(nf90_inquire_dimension(ncid,2, nfovs_name, nfovs) )
      call check(nf90_inquire_dimension(ncid,3, nscans_name, nscans) )

      write(*, *) trim(nchannel_name), "=",  nchannel, &
                  trim(nfovs_name), "=", nfovs,  & 
                  trim(nscans_name), "=", nscans 

      allocate(bt_data(nchannel, nfovs, nscans) )
      allocate(lat(nfovs, nscans) )
      allocate(lon(nfovs, nscans) )
      allocate(BeamTime(nfovs, nscans) )
      allocate(AD_Indicator(nfovs, nscans) )
      allocate(SatelliteZenithAngle(nfovs, nscans) )
      allocate(SolarZenithAngle(nfovs, nscans) )

      call check(nf90_inq_varid(ncid, "AntennaTemperature", at_varid) )
      call check(nf90_get_var(ncid,at_varid,bt_data))
      call check(nf90_inq_varid(ncid, "Longitude", lon_varid) )
      call check(nf90_get_var(ncid,lon_varid,lon))
      call check(nf90_inq_varid(ncid, "Latitude", lat_varid) )
      call check(nf90_get_var(ncid,lat_varid,lat))
      call check(nf90_inq_varid(ncid, "BeamTime", btime_varid) )
      call check(nf90_get_var(ncid,btime_varid,BeamTime))
      call check(nf90_inq_varid(ncid, "Ascending_Descending_Indicator", ad_varid) )
      call check(nf90_get_var(ncid,ad_varid,AD_Indicator))
      call check(nf90_inq_varid(ncid, "SatelliteZenithAngle", any_varid) )
      call check(nf90_get_var(ncid,any_varid, SatelliteZenithAngle))
      call check(nf90_inq_varid(ncid, "SolarZenithAngle", any_varid) )
      call check(nf90_get_var(ncid,any_varid, SolarZenithAngle))

      call check(nf90_close(ncid)) 

      write(*, *) "BeamTime(1, 1)=", BeamTime(1, 1) 
       ! BeamTime is in microsec, and starts from 1958/1/1.
       ![tiany@rain cdr-atms-combine]$ date -u -d "1958/1/1" +%s
       !-378691200
       epoch= BeamTime(1, 1)/1000000-378691200
       call epoch_to_datetime(epoch, timestamp)
       write(*, *)trim(timestamp)

      Do iscan=1, nscans
          epoch= BeamTime(1, iscan)/1000000-378691200
          call epoch_to_datetime(epoch, timestamp)
          str_scantime(iscan) = trim(timestamp)  
          !write(*, *) "str_scantime = ", str_scantime(iscan) 
          ! convert microsec since 58 to sec since 98: 
          !  date -u -d "1998/1/1" +%s 
          !  883612800
          time_tai93(iscan)=BeamTime(1, iscan)/1000000 - (378691200+883612800) 
      End Do 

      ! stuff fcdr data structure 
      !YDT lat_a1_1(1:nfovs, 1:nscans) = lat
      lat_a1_1(1:nfovs, 1:nscans) = lat
      lon_a1_1(1:nfovs, 1:nscans) = lon
      lat_a1_2(1:nfovs, 1:nscans) = lat
      lon_a1_2(1:nfovs, 1:nscans) = lon
      lat_a2(1:nfovs, 1:nscans) = lat
      lon_a2(1:nfovs, 1:nscans) = lon
      lza_a1_1(1:nfovs, 1:nscans) = SatelliteZenithAngle ! goes to earth_angle_of_incidence_a1_1
      !YDT sza(1:nfovs, 1:nscans) = SolarZenithAngle ! goes to solar_zenith_angle
      sza(1:nfovs, 1:nscans) = SolarZenithAngle ! goes to solar_zenith_angle
      
      ! Test: send the first 4 channels of bt_data to at(MAXSCANLINE_A,
      ! NUMSPOT_A, NUMCHAN_A). Note the reversed order of dimensions
   
      ! ATMOS channles: https://www.star.nesdis.noaa.gov/jpss/ATMS.php 
      !          1	23.8	0.27	1	0.7	6.3	5.2	QV
      !          2	31.4	0.18	1	0.8	6.3	5.2	QV
      !          3	50.3	0.18	0.75	0.9	3.3	2.2	QH
      !          4	51.76	0.4	0.75	0.7	3.3	2.2	QH
      !          5	52.8	0.4	0.75	0.7	3.3	2.2	QH
      !          6	53.596±0.115	0.17	0.75	0.7	3.3	2.2	QH
      !          7	54.4	0.4	0.75	0.7	3.3	2.2	QH
      !          8	54.94	0.4	0.75	0.7	3.3	2.2	QH
      !          9	55.5	0.33	0.75	0.7	3.3	2.2	QH
      !          10	57.290344	0.33	0.75	0.75	3.3	2.2	QH
      !          11	57.290344±0.217	0.078	0.75	1.2	3.3	2.2	QH
      !          12	57.290344±0.3222±0.048	0.036	0.75	1.2	3.3	2.2	QH
      !          13	57.290344±0.3222±0.022	0.016	0.75	1.5	3.3	2.2	QH
      !          14	57.290344±0.3222±0.010	0.008	0.75	2.4	3.3	2.2	QH
      !          15	57.290344±0.3222±0.0045	0.003	0.75	3.6	3.3	2.2	QH
      !          16	88.2	2	1	0.5	3.3	2.2	QV
      !          17	165.5	3	1	0.6	2.2	1.1	QH
      !          18	183.31±7	2	1	0.8	2.2	1.1	QH
      !          19	183.31±4.5	2	1	0.8	2.2	1.1	QH
      !          20	183.31±3	1	1	0.8	2.2	1.1	QH
      !          21	183.31±1.8	1	1	0.8	2.2	1.1	QH
      !          22	183.31±1	0.5	1	0.9	2.2	1.1	QH


      !channels 1, 2, 3, 16:  23.8, 31.4, 50.3, 88.2
        Do ifov=1, NUMSPOT_A 
          Do iscan=1, nscans
             at(1, ifov, iscan) = bt_data(1, ifov, iscan) 
             at(2, ifov, iscan) = bt_data(2, ifov, iscan) 
             at(3, ifov, iscan) = bt_data(3, ifov, iscan) 
             at(4, ifov, iscan) = bt_data(16, ifov, iscan) 
          End Do 
        End Do 

      Do iscan=1, nscans
       orb_mode(iscan) = AD_Indicator(1, iscan) 
      End Do 

      call set_sw(nc_outf, nscans) ! testing 
      return 

      end 
       

subroutine get_basename(path, base)
  implicit none
  character(len=*), intent(in) :: path
  character(len=*), intent(out) :: base
  integer :: i, last_slash

  last_slash = 0
  do i = len_trim(path), 1, -1
    if (path(i:i) == "/") then
      last_slash = i
      exit
    end if
  end do

  if (last_slash > 0) then
    base = path(last_slash+1:)
  else
    base = path
  end if
end subroutine get_basename

        
      


