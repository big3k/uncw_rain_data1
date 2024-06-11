
   subroutine save_data_to_hdf5(filename, grid_rain, grid_sti, & 
                             grid_year, grid_month, grid_day, &
                             grid_hour, grid_minute, grid_second)


!5/29/2024: Added data compression 

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
      real (kind=4) :: grid_rain(nx, ny), grid_sti(nx, ny)
      real (kind=4) :: grid_year(nx, ny), grid_month(nx, ny), grid_day(nx, ny)
      real (kind=4) :: grid_hour(nx, ny), grid_minute(nx, ny), grid_second(nx, ny)
      real (kind=4) :: lon(nx, ny), lat(nx, ny) 

      ! declarations
      integer (kind=4) :: status, error, rank
      integer (hsize_t) :: dims(2)
      integer (hsize_t) :: chunk_dims(2)  ! sizes of chunked data, in number of data elements
      

      !======= choose the file and field to read
      character (len=256) :: filename
      character*100,   parameter    :: group_name = "/S1" 
      character*100,   parameter    :: rain_name = "/S1/grid_surfacePrecipitation"
      character*100,   parameter    :: sti_name = "/S1/grid_surfaceTypeIndex"
      character*100,   parameter    :: lon_name = "/S1/grid_Longitude"
      character*100,   parameter    :: lat_name = "/S1/grid_Latitude"
      integer(hid_t)                :: file_id, group_id, rain_id, sti_id
      integer(hid_t)                :: year_id, month_id, day_id
      integer(hid_t)                :: hour_id, minute_id, second_id
      integer(hid_t)                :: lon_id, lat_id 
      integer(hid_t)                :: dataspace_id
      integer(hid_t)                :: plist_id  ! Property list 

      rank=2
      dims(1)=nx
      dims(2)=ny
      chunk_dims(1:2)=1800
     
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
      ! create property and set chunk  
      call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, status) 
      call h5pset_chunk_f(plist_id, 2, chunk_dims, status)
      if (status .ne. 0) write(*, *) "Failed to set chunk sizes" 

      !Set ZLIB / DEFLATE Compression using compression level 6.
      call h5pset_deflate_f(plist_id, 6, error)

      ! create a group
      call  h5gcreate_f(file_id, group_name, group_id, status)
      if (status .ne. 0) write(*, *) "Failed to create group: ", group_name 

     ! === dataset save vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      ! create dataset rain
      call  h5dcreate_f(file_id, rain_name, H5T_NATIVE_REAL, dataspace_id, &
                       rain_id, status, dcpl_id=plist_id)

         call h5dwrite_f(rain_id, H5T_NATIVE_REAL, grid_rain, dims, status) 
         if (status .ne. 0) write(*, *) "Failed to write rain "
      
      call h5dclose_f(rain_id, status) 

      ! create dataset sti 
      call  h5dcreate_f(file_id, sti_name, H5T_NATIVE_REAL, dataspace_id, &
                       sti_id, status, dcpl_id=plist_id)

         call h5dwrite_f(sti_id, H5T_NATIVE_REAL, grid_sti, dims, status)
         if (status .ne. 0) write(*, *) "Failed to write rain "

      call h5dclose_f(sti_id, status)

     ! create dataset grid_time 
      call  h5dcreate_f(file_id, "/S1/grid_Year", H5T_NATIVE_REAL, dataspace_id, &
                       year_id, status, dcpl_id=plist_id)
         call h5dwrite_f(year_id, H5T_NATIVE_REAL, grid_year, dims, status)
      call h5dclose_f(year_id, status)
      call  h5dcreate_f(file_id, "/S1/grid_Month", H5T_NATIVE_REAL, dataspace_id, &
                       month_id, status, dcpl_id=plist_id)
         call h5dwrite_f(month_id, H5T_NATIVE_REAL, grid_month, dims, status)
      call h5dclose_f(month_id, status)
      call  h5dcreate_f(file_id, "/S1/grid_Day", H5T_NATIVE_REAL, dataspace_id, &
                       day_id, status, dcpl_id=plist_id)
         call h5dwrite_f(day_id, H5T_NATIVE_REAL, grid_day, dims, status)
      call h5dclose_f(day_id, status)
      call  h5dcreate_f(file_id, "/S1/grid_Hour", H5T_NATIVE_REAL, dataspace_id, &
                       hour_id, status, dcpl_id=plist_id)
         call h5dwrite_f(hour_id, H5T_NATIVE_REAL, grid_hour, dims, status)
      call h5dclose_f(hour_id, status)
      call  h5dcreate_f(file_id, "/S1/grid_Minute", H5T_NATIVE_REAL, dataspace_id, &
                       minute_id, status, dcpl_id=plist_id)
         call h5dwrite_f(minute_id, H5T_NATIVE_REAL, grid_minute, dims, status)
      call h5dclose_f(minute_id, status)
      call  h5dcreate_f(file_id, "/S1/grid_Second", H5T_NATIVE_REAL, dataspace_id, &
                       second_id, status, dcpl_id=plist_id)
         call h5dwrite_f(second_id, H5T_NATIVE_REAL, grid_second, dims, status)
      call h5dclose_f(second_id, status)
 
     ! lat/lon data save 
      call  h5dcreate_f(file_id, lon_name, H5T_NATIVE_REAL, dataspace_id, &
                       lon_id, status, dcpl_id=plist_id)
      if (status .ne. 0) write(*, *) "Failed to create ", lon_name

         call h5dwrite_f(lon_id, H5T_NATIVE_REAL, lon, dims, status)
         if (status .ne. 0) write(*, *) "Failed to write lon "

      call h5dclose_f(lon_id, status)

      call  h5dcreate_f(file_id, lat_name, H5T_NATIVE_REAL, dataspace_id, &
                       lat_id, status, dcpl_id=plist_id)
      if (status .ne. 0) write(*, *) "Failed to create ", lat_name

         call h5dwrite_f(lat_id, H5T_NATIVE_REAL, lat, dims, status)
         if (status .ne. 0) write(*, *) "Failed to write lat "

      call h5dclose_f(lat_id, status)

     ! === dataset save ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     ! close group
     call h5gclose_f(group_id, status) 

     ! close datasapce 
     call h5sclose_f(dataspace_id, status) 
     ! close property
     call h5pclose_f(plist_id, status)
     ! close file 
     call h5fclose_f(file_id, status) 
     ! close hdf5 handle 
     call h5close_f(status) 

    return 

end  subroutine 
