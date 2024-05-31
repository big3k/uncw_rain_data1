program test_rd 

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
      integer, parameter :: ny= 2284 

      INTEGER*2   :: s_year(ny)
      INTEGER*1   :: s_month(ny), s_day(ny), s_hour(ny), s_minute(ny), s_second(ny)


      ! declarations
      integer (kind=4) :: fid,status,astat
      integer (hsize_t) :: rank,dims(2),maxdims(2), datatype,i,j, nx
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

      i =  iargc()
      If (i.ne.3) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"reproj instrument(ATMS|MHS|AMSR2...) input_h5_file output_hdf5_file"
         stop
      End If

     call getarg(1, instrument)
     call getarg(2, filename)
     call getarg(3, ofile)
      
     call read_ScanTime(filename, s_year, s_month, s_day, s_hour, s_minute, s_second, &
                              ny)

     Do j=1, ny
       write(*, *) s_year(j), s_month(j), s_day(j), " : ",  s_hour(j), s_minute(j), s_second(j) 
     End Do 
    
end program 

