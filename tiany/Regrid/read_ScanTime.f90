
!5/31/2024 Subroutine to read Year, Month, DayofMonth, hour, minute, second 
! from "ScanTime" subgroup 

    subroutine read_ScanTime(filename, s_year, s_month, s_day, s_hour, s_minute, s_second, &
                              ny) 

!  In:    filename, ny 
!  Out:   s_year, s_month, s_day, s_hour, s_minute, s_second

      use hdf5 

      implicit none

      INCLUDE "parm_res.h"
      ! for reprojection
      
      integer (hsize_t) :: ny 
      INTEGER*2   :: s_year(ny)
      INTEGER*1   :: s_month(ny), s_day(ny), s_hour(ny), s_minute(ny), s_second(ny) 


      ! declarations
      integer (kind=4) :: status

      !======= choose the file and field to read
      character (len=256) :: filename ! input and output file names 
      integer(hid_t)                :: file_id, group_id
      integer(hid_t)                :: year_id, month_id, day_id 
      integer(hid_t)                :: hour_id, minute_id, second_id 
      integer (hsize_t) :: dims(1)

      dims(1)=ny

      !======= open the interface 
      call h5open_f(status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF interface" 
      
      call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF file" 

      !s_year
      call h5dopen_f(file_id, "/S1/ScanTime/Year", year_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: "
      call h5dread_f(year_id, H5T_STD_I16LE, s_year, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read data" 

      !s_month
      call h5dopen_f(file_id, "/S1/ScanTime/Month", month_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: "
      call h5dread_f(month_id, H5T_STD_I8LE, s_month, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read data" 

      !s_day
      call h5dopen_f(file_id, "/S1/ScanTime/DayOfMonth", day_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: "
      call h5dread_f(day_id, H5T_STD_I8LE, s_day, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read data" 

      !s_hour
      call h5dopen_f(file_id, "/S1/ScanTime/Hour", hour_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: "
      call h5dread_f(hour_id, H5T_STD_I8LE, s_hour, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read data" 

      !s_minute 
      call h5dopen_f(file_id, "/S1/ScanTime/Minute", minute_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: "
      call h5dread_f(minute_id, H5T_STD_I8LE, s_minute, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read data" 

      !s_second 
      call h5dopen_f(file_id, "/S1/ScanTime/Second", second_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: "
      call h5dread_f(second_id, H5T_STD_I8LE, s_second, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read data" 

      ! done
      call h5fclose_f(file_id, status)  
      call h5close_f(status) 
    
end subroutine 

