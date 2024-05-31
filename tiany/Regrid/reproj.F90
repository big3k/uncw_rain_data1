program reproj

!5/4/2024  fix the usage of fov_orc: input flon etc. shall be arrays
! Read GPROF retrievals and reproject to 0.1-deg lat/lon grid 

      use hdf5 

      implicit none

      INCLUDE "parm_res.h"
      ! for reprojection
      
      character (len=256) :: instrument
      real, parameter :: lat0=-89.95, lon0=-179.95, res=0.1
      integer, parameter :: nc=3600, nr=1800  ! lat/lon grid
      integer, parameter :: NFOV=96 ! ATMS 
      integer, parameter :: NOUTER=0 ! see getgprofg.f90
      integer :: ic, ir, iargc, iret
      real (kind=4), allocatable :: rain(:, :), sti(:, :), lon(:, :), lat(:, :) 
      real (kind=4) :: grid_rain(nc, nr), grid_sti(nc, nr)
      real (kind=4) :: grid_year(nc, nr), grid_month(nc, nr), grid_day(nc, nr)   
      real (kind=4) :: grid_hour(nc, nr), grid_minute(nc, nr), grid_second(nc, nr)   
      real (kind=4) :: tmp1(nc, nr), tmp2(nc, nr) 
      REAL*4    :: gprtemp4c(LTS,LN,4), w(0:NFOV+1), s(0:NFOV+1)
      REAL*4    :: gprtemp4c_sti(LTS,LN,4)
      ! scanTime fields 
      REAL*4    :: gprtemp4c_year(LTS,LN,4)
      REAL*4    :: gprtemp4c_month(LTS,LN,4)
      REAL*4    :: gprtemp4c_day(LTS,LN,4)
      REAL*4    :: gprtemp4c_hour(LTS,LN,4)
      REAL*4    :: gprtemp4c_minute(LTS,LN,4)
      REAL*4    :: gprtemp4c_second(LTS,LN,4)
      REAL*4    :: flat(0:NFOV+1), flon(0:NFOV+1), xor(0:NFOV+1), yor(0:NFOV+1)
      Real*4    :: xflat, xflon, xxor, xyor, fp, fa, fw, fs
      INTEGER*1   :: influc (LN, LTS)
      INTEGER*1   :: influc_sti (LN, LTS)
      INTEGER*1   :: influc_year (LN, LTS)
      INTEGER*1   :: influc_month (LN, LTS)
      INTEGER*1   :: influc_day (LN, LTS)
      INTEGER*1   :: influc_hour (LN, LTS)
      INTEGER*1   :: influc_minute (LN, LTS)
      INTEGER*1   :: influc_second (LN, LTS)

      INTEGER*2, allocatable   :: s_year(:) 
      INTEGER*1, allocatable   :: s_month(:), s_day(:), s_hour(:), s_minute(:), s_second(:)

      ! declarations
      integer (kind=4) :: fid,status,astat
      integer (hsize_t) :: rank,dims(2),maxdims(2), datatype,i,j, nx, ny
      character (len=255) :: dimlist

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

      i =  iargc()
      If (i.ne.3) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"reproj instrument(ATMS|MHS|AMSR2...) input_h5_file output_hdf5_file"
         stop
      End If

     call getarg(1, instrument)
     call getarg(2, filename)
     call getarg(3, ofile)
      
     w=0.0
     s=0.0
     !instrument = "ATMS"
     CALL FOV_DIM  ( NFOV, instrument, w, s, iret )

     if (iret .ne. 0) then
        write(*, *) "FOV_DIM failed"
        stop
     end if 
     ! == verify numbers in "parm_res.h" 
     write(*, *) "LN=", LN, " LTS=", LTS, " LT10=", LT10, " NNSX=", NNSX, " NNSY=", NNSY
    ! output: 
    ! LN=        3600  LTS=        1800  LT10=         180  NNSX=           0  NNSY=           0

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

      gprtemp4c = -9999.0
      gprtemp4c_sti = -9999.0
      gprtemp4c_year = -9999.0
      gprtemp4c_month = -9999.0
      gprtemp4c_day = -9999.0
      gprtemp4c_hour = -9999.0
      gprtemp4c_minute = -9999.0
      gprtemp4c_second = -9999.0

      influc = 50
      influc_sti = 50
      influc_year = 50
      influc_month = 50
      influc_day = 50
      influc_hour = 50
      influc_minute = 50
      influc_second = 50

      nx = dims(1)   ! NPOV, 96 for ATMS
      ny = dims(2) 

      write(*, *)"nx = ", nx, " ny=", ny
      ! output: 
      ! nx =                    96  ny=                 2284
      allocate(rain(nx, ny)) 
      allocate(sti(nx, ny)) 
      allocate(lat(nx, ny)) 
      allocate(lon(nx, ny)) 
      allocate(s_year(ny)) 
      allocate(s_month(ny)) 
      allocate(s_day(ny)) 
      allocate(s_hour(ny)) 
      allocate(s_minute(ny)) 
      allocate(s_second(ny)) 

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

      ! get scanTime data elements
      call read_ScanTime(filename, s_year, s_month, s_day, s_hour, s_minute, s_second, &
                              ny)

      ! naive reprojection
      do j=1, ny 
       do i=1, nx 
         if (rain(i, j) .ge. 0) then 
             ir = nint ( (lat(i, j) - lat0 )/res ) + 1
             ic = nint ( (lon(i, j) - lon0 )/res ) + 1
             !write(*, *) "lon=", lon(i, j), " lat=", lat(i, j), "ic=", ic, " ir=", ir 
             grid_rain(ic, ir) = rain(i, j) 
             grid_sti(ic, ir) = sti(i, j) 
         end if
        end do 
      end do 
     
      write(*, *) "Saving binary format ...", nc, nr
      open(22, file="test.2gd4r", form="unformatted", access="direct", recl=nc*nr*4) 
          write(22, rec=1) grid_rain 
          write(22, rec=2) grid_sti 
      close(22) 
       
      ! sophisticated reprojection: split FOV 
      ! naive reprojection
      do j=1, ny
         flon(1:NFOV)=lon(:, j)
         flat(1:NFOV)=lat(:, j)
          call FOV_ORC  ( flon, flat, ILATEXT, NCEDFRAC, NFOV, &
                              NOUTER, xor, yor, iret ) 
           if (iret .ne. 0) then
              write(*, *) "FOV_ORC failed"
              stop
           end if 
          !write(*, *) "xor=", xor, " yor=", yor, " fw=", fw, " fs=", fs, &
          !     " fp=", fp
       do i=1, nx
         if (rain(i, j) .ge. 0) then 
          fw=w(i)
          fs=s(i)
          xxor=xor(i)
          xyor=yor(i)
          xflon=flon(i)
          xflat=flat(i)

          fp=rain(i, j)
          call FOV2GC  (LN, LTS, ILATEXT, xflon, xflat, fp,  &
                             fa, xxor, xyor, fw, fs, influc, gprtemp4c, &
                             iret )
          ! YDT: examination of FOVGC() code shows that it can also handle categoritical data as input fp
          fp=sti(i, j)
          call FOV2GC  (LN, LTS, ILATEXT, xflon, xflat, fp,  &
                             fa, xxor, xyor, fw, fs, influc_sti, gprtemp4c_sti, iret )

          ! year/month/day/hour/minute/second to grid 
          fp=real(s_year(j)) 
          call FOV2GC  (LN, LTS, ILATEXT, xflon, xflat, fp,  &
                             fa, xxor, xyor, fw, fs, influc_year, gprtemp4c_year, iret )
          fp=real(s_month(j)) 
          call FOV2GC  (LN, LTS, ILATEXT, xflon, xflat, fp,  &
                             fa, xxor, xyor, fw, fs, influc_month, gprtemp4c_month, iret )
          fp=real(s_day(j)) 
          call FOV2GC  (LN, LTS, ILATEXT, xflon, xflat, fp,  &
                             fa, xxor, xyor, fw, fs, influc_day, gprtemp4c_day, iret )
          fp=real(s_hour(j)) 
          call FOV2GC  (LN, LTS, ILATEXT, xflon, xflat, fp,  &
                             fa, xxor, xyor, fw, fs, influc_hour, gprtemp4c_hour, iret )
          fp=real(s_minute(j)) 
          call FOV2GC  (LN, LTS, ILATEXT, xflon, xflat, fp,  &
                             fa, xxor, xyor, fw, fs, influc_minute, gprtemp4c_minute, iret )
          fp=real(s_second(j)) 
          call FOV2GC  (LN, LTS, ILATEXT, xflon, xflat, fp,  &
                             fa, xxor, xyor, fw, fs, influc_second, gprtemp4c_second, iret )

         end if 
       end do 
     end do 

     ! flip first two dimensions of gprtemp4c
      do j=1, nr
       do i=1, nc
        grid_rain(i, j)=gprtemp4c(j, i, 1)
        grid_sti(i, j)=gprtemp4c_sti(j, i, 1)
        grid_year(i, j)=gprtemp4c_year(j, i, 1)
        grid_month(i, j)=gprtemp4c_month(j, i, 1)
        grid_day(i, j)=gprtemp4c_day(j, i, 1)
        grid_hour(i, j)=gprtemp4c_hour(j, i, 1)
        grid_minute(i, j)=gprtemp4c_minute(j, i, 1)
        grid_second(i, j)=gprtemp4c_second(j, i, 1)
       end do 
      end do 

      open(24, file="new_test.4gd4r", form="unformatted", access="direct", recl=nc*nr*4) 
          write(24, rec=1) grid_rain
          write(24, rec=2) grid_sti 
          write(24, rec=3) grid_year
          write(24, rec=4) grid_hour
      close(24) 
      ! save to HDF5 
      call save_data_to_hdf5(ofile, grid_rain, grid_sti, &
                             grid_year, grid_month, grid_day, &
                             grid_hour, grid_minute, grid_second) 
    
end program reproj 

