
! Read a nc file and project onto 0.1x0.1 degree for visualization

      program reproj

      use netcdf 
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
      integer :: ic, ir, iz , i, j, k
      character(len=1064) :: nc_file 
      character(len=1064) :: outf 
      character(len=1064) :: nchannel_name, nfovs_name, nscans_name 
      ! NC data buffer
      real*4, allocatable :: grid_bt(:, :, :) 

      real*4, allocatable :: bt_data(:, :, :), lon(:, :), lat(:, :) 
      integer*4, allocatable :: id2data(:, :) 
      integer :: ncid, status, nrec
      integer :: d2dims(2), d3dims(3) 
      INTEGER(KIND=4) :: x_dimid, y_dimid, z_dimid
      INTEGER(KIND=4) :: at_varid, lon_varid, lat_varid, sza_varid 
      INTEGER(KIND=4) :: bt_varid, adi_varid

      ndir =  iargc()
      If (ndir.ne.2) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"reproj_data. input_nc_file output_bin_file" 
         stop
      End If
      call getarg(1, nc_file)
      call getarg(2, outf) 
 
      call set_sw(outf, 1000) ! testing numscan=1000
      stop

      
      !open netcdf for saving variables 
      call check(nf90_open(nc_file, nf90_nowrite, ncid))
      call check(nf90_inquire_dimension(ncid,1, nchannel_name, nchannel) )
      call check(nf90_inquire_dimension(ncid,2, nfovs_name, nfovs) )
      call check(nf90_inquire_dimension(ncid,3, nscans_name, nscans) )

      write(*, *) "nchannel=", nchannel, " nfovs=", nfovs, " nscans=", nscans 

      allocate(bt_data(nchannel, nfovs, nscans) )
      allocate(lat(nfovs, nscans) )
      allocate(lon(nfovs, nscans) )

      call check(nf90_inq_varid(ncid, "AntennaTemperature", at_varid) )
      call check(nf90_get_var(ncid,at_varid,bt_data))
      call check(nf90_inq_varid(ncid, "Longitude", lon_varid) )
      call check(nf90_get_var(ncid,lon_varid,lon))
      call check(nf90_inq_varid(ncid, "Latitude", lat_varid) )
      call check(nf90_get_var(ncid,lat_varid,lat))
      call check(nf90_close(ncid)) 

      allocate(grid_bt(nc, nr, nchannel)) 
      grid_bt=-9999.0
      do k=1, nchannel
       do j=1, nfovs 
         do i=1, nscans 
            ! lat/lon has -999.299988 values as kind of undef
            if ( lat(j, i) .gt. -90.0 .and. lon(j, i) .gt. -180.0) then 
             ir = nint ( (lat(j, i) - lat0 )/res ) + 1
             ic = nint ( (lon(j, i) - lon0 )/res ) + 1
             !write(*, *) "k=", k, " j=", j, " i=", i
             !write(*, *) "lon=", lon(j, i), " lat=", lat(j, i), "ic=", ic, " ir=", ir
             grid_bt(ic, ir, k) = bt_data(k, j, i) 
           end if
        end do
       end do
      end do
      write(*, *) "Saving binary format ...", nc, nr
      open(22, file=outf, form="unformatted", access="direct", recl=nc*nr*4)
      do k=1, nchannel
      !do k=1, 1 ! only one channel to test
        write(22, rec=k) grid_bt(:, :, k) 
      end do 
      close(22)

      ! dumping out raw file. can not catnate 
      open(24, file=trim(outf)//".raw", form="unformatted", access="direct", &
           recl=4)
      nrec=1
      do k=1, nchannel
        do j=1, nfovs 
          do i=1, nscans 
           write(24, rec=nrec) bt_data(k, j, i) 
           nrec=nrec+1
         end do 
        end do 
      end do 
      close(24)
      
      return 

      end 
       
        
      


