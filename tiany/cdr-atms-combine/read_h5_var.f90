
! Read a variable from given h5 file 
! Max rank of the variable is assumed to be 3

      subroutine read_h5_var(h5_file, var_name, h5_data, nx, ny, nz) 

      use hdf5

      implicit none
      INCLUDE "parms.h"
 
      integer :: ndir, nf, jf, ibin, nf_in_bin
      character(len=*), intent(in) :: h5_file, var_name 
      real (kind=4), allocatable   :: var_data(:, :, :) 
      real (kind=4)   :: h5_data(MAX_NX, MAX_NY, MAX_NZ) 
      integer :: nx, ny, nz, status, ix, iy, iz

      integer(hid_t)    :: file_id, data_id, dataspace 
      integer (hsize_t) :: rank,dims(3),maxdims(3) 
 
      dims=1 

      write(*, *) "reading ", trim(h5_file), " var=", trim(var_name) 
      !======= open the interface
      call h5open_f(status)
      if (status .ne. 0) write(*, *) "Failed to open HDF interface"
      
      call h5fopen_f(h5_file, H5F_ACC_RDONLY_F, file_id, status)
      if (status .ne. 0) write(*, *) "Failed to open HDF file"
 
      call h5dopen_f(file_id, var_name, data_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: "
       
      ! get the dimension of the data record
      call h5dget_space_f(data_id, dataspace, status)
      if (status .ne. 0) write(*, *) "Failed to get dataspace id"
 
      CALL h5sget_simple_extent_dims_f(dataspace, dims, maxdims, status)
      if (status .lt. 0) write(*, *) "Failed to get dims, status=", status
      nx=dims(1) 
      ny=dims(2) 
      nz=dims(3) 
      allocate(var_data(nx, ny, nz)) 

      !call h5dread_f(data_id, H5T_IEEE_F32LE, h5_data, dims, status)
      call h5dread_f(data_id, H5T_IEEE_F32LE, var_data, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read data"
 
      Do iz=1, nz
      Do iy=1, ny
        Do ix=1, nx
          !write(*, *) var_data(ix, iy) 
          h5_data(ix, iy, iz) = var_data(ix, iy, iz) 
        End Do 
      End Do
      End Do

      call h5fclose_f(file_id, status)
      call h5close_f(status)
 
      deallocate(var_data) 
       
      return 

      end 
       
        
      


