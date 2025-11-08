      subroutine epoch_to_datetime(epoch, datetime_str)
       use iso_c_binding, only: c_int64_t
       implicit none

       ! Inputs
       integer(c_int64_t), intent(in) :: epoch

       ! Outputs
       character(len=*), intent(out) :: datetime_str

       ! Local time variables
       integer :: year, month, day, hour, minute, second
       integer :: days, leap, i
       integer, dimension(12) :: month_days = [31,28,31,30,31,30,31,31,30,31,30,31]
       integer :: secs_in_day, secs_in_hour, secs_in_minute

       ! Constants
       parameter (secs_in_day = 86400, secs_in_hour = 3600, secs_in_minute = 60)

       ! Time breakdown
       integer :: remaining_secs, days_in_year

       ! Start from 1970-01-01
       year = 1970
       days = epoch / secs_in_day
       remaining_secs = epoch - days * secs_in_day

       ! Convert to calendar date
       do
              leap = 0
              if (mod(year,4) == 0 .and. (mod(year,100) /= 0 .or. mod(year,400) == 0)) leap = 1
              days_in_year = 365 + leap
              if (days < days_in_year) exit
              days = days - days_in_year
              year = year + 1
       end do

       ! Adjust month for leap year
       if (leap == 1) month_days(2) = 29

       month = 1
       do i = 1, 12
              if (days < month_days(i)) exit
              days = days - month_days(i)
              month = month + 1
       end do

       day = days + 1

       ! Now convert remaining seconds to time
       hour = remaining_secs / secs_in_hour
       remaining_secs = mod(remaining_secs, secs_in_hour)
       minute = remaining_secs / secs_in_minute
       second = mod(remaining_secs, secs_in_minute)

       ! Format output: YYYY-MM-DDTHH:MM:SS
       write(datetime_str, '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,"-",I2.2,"-",I2.2)') &
                  year, month, day, hour, minute, second

     end subroutine epoch_to_datetime

