program reproj

!5/4/2024  fix the usage of fov_orc: input flon etc. shall be arrays
! Read GPROF retrievals and reproject to 0.1-deg lat/lon grid 

      use hdf5 

      implicit none

      INCLUDE "parm_res.h"
      ! for reprojection
      
      character (len=256) :: instrument
      real, parameter :: lat0=-89.95, lon0=-179.95, res=0.1
      real, parameter :: Re=6378.0 ! Earth radius in km. 
      real, parameter :: Pi=3.14159265
      real :: deg_per_km  ! lat/lon degree per km
      integer, parameter :: nc=3600, nr=1800  ! lat/lon grid
      integer, parameter :: NFOV=96 ! ATMS 
      integer, parameter :: NOUTER=0 ! see getgprofg.f90
      integer :: ic, ir, iargc, iret
      real (kind=4), allocatable :: rain(:, :), sti(:, :), lon(:, :), lat(:, :) 
      real (kind=4) :: orain(nc, nr), osti(nc, nr) 
      real (kind=4) :: tmp1(nc, nr), tmp2(nc, nr) 
      REAL*4    :: gprtemp4c(LTS,LN,4), w(0:NFOV+1), s(0:NFOV+1)
      REAL*4    :: flat(0:NFOV+1), flon(0:NFOV+1), xor(0:NFOV+1), yor(0:NFOV+1)
      Real*4    :: xflat, xflon, xxor, xyor, fp, fa, fw, fs
      Real*4    :: theta  ! tilt of the FOV, in deg
      Real*4    :: beta  ! angle of ellipse 
      Real*4    :: dx, dy ! distance from center of ellipse, in deg
      INTEGER*1   :: influc (LN, LTS)


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
      If (i.ne.1) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"draw_fov input_h5_file" 
         stop
      End If

     call getarg(1, filename)
      
     w=0.0
     s=0.0
     instrument = "ATMS"
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
     
      !write(*, *) "Saving binary format ...", nc, nr
      !open(22, file=ofile, form="unformatted", access="direct", recl=nc*nr*4) 
      !    write(22, rec=1) orain 
      !    write(22, rec=2) osti 
      !close(22) 
       
      ! sophisticated reprojection: split FOV 
      ! naive reprojection
      !do j=1, ny
      !do j=1500, 1500  ! select a scan (i.e., ~lat)  > do_draw_fov.gs
      !do j=500, 500  ! select a scan (i.e., ~lat)  > do_draw_fov2.gs
      do j=1000, 1000  ! select a scan (i.e., ~lat)  > do_draw_fov3.gs
         flon(1:NFOV)=lon(:, j)
         flat(1:NFOV)=lat(:, j)
          call FOV_ORC  ( flon, flat, ILATEXT, NCEDFRAC, NFOV, &
                              NOUTER, xor, yor, iret ) 
           if (iret .ne. 0) then
              write(*, *) "FOV_ORC failed"
              stop
           end if 

       !write(*, *) "scan, w, s, lat, lon, xor, yor" 
       !do i=1, nx
       do i=5, 92 ! compute each fov,  skip edges 
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
          write(*, '(A)', advance="no") "'plot_poly " 
          do beta=0, 360, 10
            dx=0.5*fs*cosd(real(beta)-theta)*deg_per_km/cosd(xflat)
            dy=0.5*fw*sind(real(beta)-theta)*deg_per_km
            write(*, '(2F10.4)', advance="no") xflon+dx, xflat+dy
         end do 
         write(*, *) "'" 
          
       end do  ! along scan 
     end do  ! across scan

    
end program reproj 

