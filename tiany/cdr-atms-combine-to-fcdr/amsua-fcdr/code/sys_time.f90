! sys_time.f90
module sys_time
  implicit none

  type :: Sys_Time_TYPE
    integer :: sys_year
    integer :: sys_mon
    integer :: sys_mday
    integer :: sys_hour
    integer :: sys_min
    integer :: sys_sec
    integer :: sys_jday
  end type Sys_Time_TYPE

end module sys_time
