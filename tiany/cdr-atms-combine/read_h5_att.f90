
! Read an attribute from given h5 file 
! Assuming 1 data point, H5T_STD_U8LE type

      subroutine read_h5_att(h5_file, group_name, att_name, att_data) 

      use hdf5

      implicit none
      INCLUDE "parms.h"
 
      integer :: ndir, nf, jf, ibin, nf_in_bin, att_data
      character(len=*), intent(in) :: h5_file, group_name, att_name 
      integer :: nx, ny, nz, status, ix, iy, iz

      integer(hid_t)    :: file_id, att_id, dataspace 
      integer (hsize_t) :: rank,dims(1),maxdims(3) 
 
      dims=1 

      !write(*, *) "reading ", trim(h5_file), " att=", trim(att_name) 
      !======= open the interface
      call h5open_f(status)
      if (status .ne. 0) write(*, *) "Failed to open HDF interface"
      
      call h5fopen_f(h5_file, H5F_ACC_RDONLY_F, file_id, status)
      if (status .ne. 0) write(*, *) "Failed to open HDF file"
 
      call h5aopen_by_name_f(file_id, group_name, att_name, att_id, status)
      if (status .ne. 0) write(*, *) "Failed to get attribute: "

      call h5aread_f(att_id, H5T_STD_U8LE, att_data, dims, status) 
      if (status .ne. 0) write(*, *) "Failed to get attribute: "
       
      call h5aclose_f(att_id, status)
      call h5fclose_f(file_id, status)
      call h5close_f(status)
       
      return 

      end 
       
        
      


