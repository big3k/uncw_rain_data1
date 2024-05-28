

     subroutine save_data_to_hdf5(filename, rain, sti, grid_time) 

!5/26/2024  Save 0.1-degree data to HDF5. 
! Dimensions: nx, ny
! Variables
!  grid_surfacePrecipitation (ny, nx)
!  grid_surfaceTypeIndex (ny, nx) 
!  grid_time (ny, nx) 
!  grid_Longitude (ny, nx)  [-180, 180]
!  grid_Latitude (ny, nx)   [-90, 90]
!
      use hdf5 

      implicit none

      real, parameter :: lat0=-89.95, lon0=-179.95, res=0.1
      integer, parameter :: nx=3600, ny=1800  ! lat/lon grid
      integer :: ix, iy 
      real (kind=4) :: rain(nx, ny), sti(nx, ny), grid_time(nx, ny)
      real (kind=4) :: lon(nx, ny), lat(nx, ny) 

      ! declarations
      integer (kind=4) :: status, error, rank
      integer (hsize_t) :: dims(2)

      !======= choose the file and field to read
      character (len=256) :: filename
      character*100,   parameter    :: group_name = "/S1" 
      character*100,   parameter    :: rain_name = "/S1/grid_surfacePrecipitation"
      character*100,   parameter    :: sti_name = "/S1/grid_surfaceTypeIndex"
      character*100,   parameter    :: time_name = "/S1/grid_time"
      character*100,   parameter    :: lon_name = "/S1/grid_Longitude"
      character*100,   parameter    :: lat_name = "/S1/grid_Latitude"
      integer(hid_t)                :: file_id, group_id, rain_id, sti_id, time_id
      integer(hid_t)                :: lon_id, lat_id 
      integer(hid_t)                :: dataspace_id

      rank=2
      dims(1)=nx
      dims(2)=ny
     
      ! fill lat/lon values 
       Do iy=1, ny
         lat(:, iy) = lat0 + (iy-1)*res
       End Do
       Do ix=1, nx
         lon(ix, :) = lon0 + (ix-1)*res 
       End Do 
         
      call h5open_f(status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF interface" 
      
      ! create file 
      call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, status)
      if (status .ne. 0) write(*, *) "Failed to create HDF file" 
      
      ! create datasapce 
      call h5screate_simple_f(rank, dims, dataspace_id, status) 
      if (status .ne. 0) write(*, *) "Failed to create HDF dataspace" 

      ! create a group
      call  h5gcreate_f(file_id, group_name, group_id, status)
      if (status .ne. 0) write(*, *) "Failed to create group: ", group_name 

     ! === dataset save vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      ! create dataset rain
      call  h5dcreate_f(file_id, rain_name, H5T_NATIVE_REAL, dataspace_id, &
                       rain_id, error)

         call h5dwrite_f(rain_id, H5T_NATIVE_REAL, rain, dims, status) 
         if (status .ne. 0) write(*, *) "Failed to write rain "
      
      call h5dclose_f(rain_id, status) 

      ! create dataset sti 
      call  h5dcreate_f(file_id, sti_name, H5T_NATIVE_REAL, dataspace_id, &
                       sti_id, error)

         call h5dwrite_f(sti_id, H5T_NATIVE_REAL, sti, dims, status)
         if (status .ne. 0) write(*, *) "Failed to write rain "

      call h5dclose_f(sti_id, status)

     ! create dataset grid_time 
      call  h5dcreate_f(file_id, time_name, H5T_NATIVE_REAL, dataspace_id, &
                       time_id, status)

         call h5dwrite_f(time_id, H5T_NATIVE_REAL, grid_time, dims, status)
         if (status .ne. 0) write(*, *) "Failed to write grid_time "

      call h5dclose_f(time_id, status)
 
     ! lat/lon data save 
      call  h5dcreate_f(file_id, lon_name, H5T_NATIVE_REAL, dataspace_id, &
                       lon_id, status)
      if (status .ne. 0) write(*, *) "Failed to create ", lon_name

         call h5dwrite_f(lon_id, H5T_NATIVE_REAL, lon, dims, status)
         if (status .ne. 0) write(*, *) "Failed to write lon "

      call h5dclose_f(lon_id, status)

      call  h5dcreate_f(file_id, lat_name, H5T_NATIVE_REAL, dataspace_id, &
                       lat_id, status)
      if (status .ne. 0) write(*, *) "Failed to create ", lat_name

         call h5dwrite_f(lat_id, H5T_NATIVE_REAL, lat, dims, status)
         if (status .ne. 0) write(*, *) "Failed to write lat "

      call h5dclose_f(lat_id, status)

     ! === dataset save ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     ! close group
     call h5gclose_f(group_id, status) 

     ! close datasapce 
     call h5sclose_f(dataspace_id, status) 
     ! close file 
     call h5fclose_f(file_id, status) 
     ! close hdf5 handle 
     call h5close_f(status) 

    return 

end  subroutine 

