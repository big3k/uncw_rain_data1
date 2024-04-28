program reproj

! Read GPROF retrievals and reproject to 0.1-deg lat/lon grid 

      use hdf5 

      implicit none

      ! for reprojection
      real, parameter :: lat0=-89.95, lon0=-179.95, res=0.1
      integer, parameter :: nc=3600, nr=1800  ! lat/lon grid
      integer :: ic, ir, iargc
      real (kind=4), allocatable :: rain(:), lon(:), lat(:) 
      real (kind=4) :: orain(nc, nr) 

      ! declarations
      integer (kind=4) :: fid,swid,status,astat
      integer (hsize_t) :: rank,dims(2),maxdims(2), datatype,i,j, nx
      character (len=255) :: dimlist
      integer (kind=4), allocatable :: start(:),stride(:)

      !======= choose the file and field to read
      character (len=256) :: filename, ofile ! input and output file names 
      character*100,   parameter    :: group_name = "S1" 
      character*100,   parameter    :: gprof = "surfacePrecipitation"
      character*100,   parameter    :: sti = "surfaceTypeIndex"
      character*100,   parameter    :: lon_name = "Longitude"
      character*100,   parameter    :: lat_name = "Latitude"
      integer(hid_t)                :: file_id, group_id, gprof_id, sti_id
      integer(hid_t)                :: lon_id, lat_id 
      integer(hid_t)                :: dataspace

      i =  iargc()
      If (i.ne.2) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"reproj input_h5_file output_bin_file"
         stop
      End If

     call getarg(1, filename)
     call getarg(2, ofile)
      
      !======= open the interface 
      call h5open_f(status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF interface" 
      
      call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF file" 
      
      call h5gopen_f(file_id,group_name,group_id, status)
      if (status .ne. 0) write(*, *) "Failed to get group: ", group_name 

      call h5dopen_f(group_id, gprof, gprof_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: ", gprof

      call h5dget_space_f(gprof_id, dataspace, status)
      if (status .ne. 0) write(*, *) "Failed to get dataspace id" 

      CALL h5sget_simple_extent_dims_f(dataspace, dims, maxdims, status)
      if (status .lt. 0) write(*, *) "Failed to get dims, status=", status 

      write(*, *) "dims=", dims, " maxdims=", maxdims


      call h5fclose_f(file_id, status)  
      call h5close_f(status) 

       
end program reproj 
