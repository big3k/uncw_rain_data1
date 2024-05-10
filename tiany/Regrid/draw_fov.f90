program draw_fov

! Generate rainrate map over 0.1-deg lat/lon grid with naive interpolation, and 
! a .gs file for GrADS to plot FOV on top of the grid. 

! usage: 
!  draw_fov sensor_name scan_file.HDF5 
!
!  It will generate five output files, 
!   <sensor_name>.2gd4r
!   <sensor_name>_<1-4>.gs  -- fov plot file at each of 4 predefined latitudes 

!5/4/2024  fix the usage of fov_orc: input flon etc. shall be arrays
! Read GPROF retrievals and reproject to 0.1-deg lat/lon grid 

      use hdf5 

      implicit none

      INCLUDE "parm_res.h"
      ! for reprojection
      
      character (len=256) :: instrument, plot_file
      real, parameter :: lat0=-89.95, lon0=-179.95, res=0.1
      real, parameter :: Re=6378.0 ! Earth radius in km. 
      real, parameter :: Pi=3.14159265
      real :: deg_per_km  ! lat/lon degree per km
      integer, parameter :: nc=3600, nr=1800  ! lat/lon grid
      integer :: NFOV ! get from get_NFOV() 
      real :: clat1, clat2  ! lat of center FOV 
      integer, parameter :: NOUTER=0 ! see getgprofg.f90
      integer :: ic, ir, iargc, iret
      real (kind=4), allocatable :: rain(:, :), sti(:, :), lon(:, :), lat(:, :) 
      real (kind=4) :: orain(nc, nr), osti(nc, nr) 
      real (kind=4) :: tmp1(nc, nr), tmp2(nc, nr) 
      ! LN=nc, LTS=nr
      REAL*4    :: gprtemp4c(LTS,LN,4)
      real*4, allocatable ::  w(:), s(:)   ! 0:NFOV+1
      REAL*4, allocatable :: flat(:), flon(:), xor(:), yor(:) ! 0:NFOV+1
      INTEGER*4      ::  scans_per_orbit
      real*4      :: rate_thresh

      Real*4    :: xflat, xflon, xxor, xyor, fp, fa, fw, fs
      Real*4    :: theta  ! tilt of the FOV, in deg
      Real*4    :: beta  ! angle of ellipse 
      Real*4    :: dx, dy ! distance from center of ellipse, in deg
      INTEGER*1   :: influc (LN, LTS)
      ! generate fov plots at three latitudes
      real*4 :: plot_lat(4) = (/0.0, 25.0, 50.0, 75.0/)

      ! declarations
      integer (kind=4) :: fid,status,astat
      integer (hsize_t) :: rank,dims(2),maxdims(2), datatype,i,j, nx, ny
      character (len=255) :: dimlist
      integer (kind=4), allocatable :: start(:),stride(:)

      !======= choose the file and field to read
      character (len=256) :: filename, ofile ! input and output file names 
      character*100,   parameter    :: group_name = "S1" 
      character*100,   parameter    :: rain_name = "surfacePrecipitation"
      character*100,   parameter    :: sti_name = "surfaceTypeIndex"
      character*100,   parameter    :: lon_name = "Longitude"
      character*100,   parameter    :: lat_name = "Latitude"
      integer(hid_t)                :: file_id, group_id, rain_id, sti_id
      integer(hid_t)                :: lon_id, lat_id 
      integer(hid_t)                :: dataspace

      deg_per_km = 360.0/(2.0*Pi*Re) 

      i =  iargc()
      If (i.ne.2) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"draw_fov instrument input_h5_file" 
         stop
      End If

     call getarg(1, instrument)
     call getarg(2, filename)

     call get_NFOV(instrument, NFOV, scans_per_orbit, &
                            rate_thresh)
      
     allocate(w(0:NFOV+1))
     allocate(s(0:NFOV+1))
     allocate(flat(0:NFOV+1))
     allocate(flon(0:NFOV+1))
     allocate(xor(0:NFOV+1))
     allocate(yor(0:NFOV+1))

     w=0.0
     s=0.0

     CALL FOV_DIM  ( NFOV, instrument, w, s, iret )

     if (iret .ne. 0) then
        write(*, *) "FOV_DIM failed"
        stop
     end if 
     ! == verify numbers in "parm_res.h" 
     !write(*, *) "LN=", LN, " LTS=", LTS, " LT10=", LT10, " NNSX=", NNSX, " NNSY=", NNSY
      !======= open the interface 
      call h5open_f(status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF interface" 
      
      call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF file" 
      
      call h5gopen_f(file_id,group_name,group_id, status)
      if (status .ne. 0) write(*, *) "Failed to get group: ", group_name 

      call h5dopen_f(group_id, rain_name, rain_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: ", rain_name

      ! get the dimension of the data record 
      call h5dget_space_f(rain_id, dataspace, status)
      if (status .ne. 0) write(*, *) "Failed to get dataspace id" 

      CALL h5sget_simple_extent_dims_f(dataspace, dims, maxdims, status)
      if (status .lt. 0) write(*, *) "Failed to get dims, status=", status 

      orain = -9999.0
      gprtemp4c = -9999.0
      influc = 50

      nx = dims(1)   ! NPOV, 96 for ATMS
      ny = dims(2) 

      !write(*, *)"nx = ", nx, " ny=", ny
      allocate(rain(nx, ny)) 
      allocate(sti(nx, ny)) 
      allocate(lat(nx, ny)) 
      allocate(lon(nx, ny)) 

      ! read rain
      call h5dread_f(rain_id, H5T_NATIVE_REAL, rain, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read rain" 

      ! read sti
      call h5dopen_f(group_id, sti_name, sti_id, status)
      if (status .ne. 0) write(*, *) "Failed to get sti_id" 

      call h5dread_f(sti_id, H5T_NATIVE_REAL, sti, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read rain" 

      ! read lon
      call h5dopen_f(group_id, lon_name, lon_id, status)
      if (status .ne. 0) write(*, *) "Failed to get lon_id" 

      call h5dread_f(lon_id, H5T_NATIVE_REAL, lon, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read lon" 

      ! read lat
      call h5dopen_f(group_id, lat_name, lat_id, status)
      if (status .ne. 0) write(*, *) "Failed to get lat_id" 

      call h5dread_f(lat_id, H5T_NATIVE_REAL, lat, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read lat" 

      ! done
      call h5fclose_f(file_id, status)  
      call h5close_f(status) 

      ! naive reprojection
      do j=1, ny 
       do i=1, nx 
         if (rain(i, j) .ge. 0) then 
             ir = nint ( (lat(i, j) - lat0 )/res ) + 1
             ic = nint ( (lon(i, j) - lon0 )/res ) + 1
             !write(*, *) "lon=", lon(i, j), " lat=", lat(i, j), "ic=", ic, " ir=", ir 
             orain(ic, ir) = rain(i, j) 
             osti(ic, ir) = sti(i, j) 
         end if
        end do 
      end do 
     
      ofile=trim(instrument)//".2gd4r" 
      !write(*, *) "Saving binary format ...", nc, nr
      open(22, file=ofile, form="unformatted", access="direct", recl=nc*nr*4) 
          write(22, rec=1) orain 
          write(22, rec=2) osti 
      close(22) 
       
      ! same FOV patterns at three latitudes 
      ! naive reprojection
      !do j=1, ny
      do j=2, ny-1
         flon(1:NFOV)=lon(:, j)
         flat(1:NFOV)=lat(:, j)
          clat1=lat(nint(0.5*NFOV), j-1)  ! center lat of last scan
          clat2=flat(nint(0.5*NFOV))  ! center lat of the scan
          !................................
          do ic=1, 4  ! ............
           !---- plot only at e latitudes ----------------------
           if ( clat1 .gt. -90.0 .and. clat1 .lt. 90.0 .and. &
                clat2 .gt. -90.0 .and. clat2 .lt. 90.0 .and. &
                (plot_lat(ic) - clat1)*( plot_lat(ic)- clat2) .lt. 0.0 ) then
             plot_file=trim(instrument)//"_"//trim(char(ic+ichar("0")))//".gs"
             write(*, *) "ic=", ic,  " clat1=", clat1, " clat2=", clat2
             open(24, file=plot_file, form="formatted") 

                call FOV_ORC  ( flon, flat, ILATEXT, NCEDFRAC, NFOV, &
                              NOUTER, xor, yor, iret ) 
                 if (iret .ne. 0) then
                   write(*, *) "FOV_ORC failed"
                   stop
                 end if 

                !write(*, *) "scan, w, s, lat, lon, xor, yor" 
                !do i=1, nx
                do i=1, NFOV ! compute each fov,  skip edges 
                   fp=rain(i, j)
                   fw=w(i) ! FOV width (cross-scan) in km, short axis 
                   fs=s(i) ! FOV length (along-scan) in km, long-axis 
                   xxor=xor(i)
                   xyor=yor(i)
                   xflon=flon(i)
                   xflat=flat(i)

                   theta=atand(xxor/xyor)  ! see fov_orc.f90
                                  !        yscan   = fy0 (i+1) - fy0 (i-1)
                                  !        xor (i) =  yscan
                                  !        yor (i) = -xscan

                   !write(*, '(I3,7F10.2)') i, fw, fs, xflat, xflon, xxor, xyor, theta
                   ! create points on the edge of the fov
                   !
                   if ( xflat .gt. -90.0 .and. xflat .lt. 90.0 ) then 
                   write(24, '(A)', advance="no") "'plot_poly " 
                     do beta=0, 360, 10
                       dx=0.5*fs*cosd(real(beta)-theta)*deg_per_km/cosd(xflat)
                       dy=0.5*fw*sind(real(beta)-theta)*deg_per_km
                       write(24, '(2F10.4)', advance="no") xflon+dx, xflat+dy
                     end do 
                   write(24, *) "'" 
                   end if 
          
                end do  ! along scan 
                close(24)
           end if 
           !---- end plot only at e latitudes ----------------------
          end do 
          !................................
     end do  ! across scan

    
end program draw_fov 

