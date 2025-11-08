! geo_time.f90
module geo_time
  implicit none

  type :: GEO_STIME_TYPE
    integer :: year
    character :: month, dom
    character :: hour, minute, second
    integer :: doy
  end type GEO_STIME_TYPE

end module geo_time
