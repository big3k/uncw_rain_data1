

      program test 
      integer (kind=8) big_data
      integer (kind=4) new_data, new_sec
      integer (kind=4), parameter   :: time_offset = 1325376000 
      real*8 ms

      big_data=1709423766684743
      ms=1000000.0

      new_data=nint(big_data/ms) 

      new_sec=nint(big_data/ms)-time_offset
      write(*, *) "big_data=", big_data, " new_data=", new_data
      write(*, *) "new_sec=", new_sec 

     end 

      
