

      program test 
      integer (kind=8) big_data
      integer (kind=4) new_data
      real*8 ms

      big_data=1709423766684743
      ms=1000000.0

      new_data=nint(big_data/ms) 

      write(*, *) "big_data=", big_data, " new_data=", new_data

     end 

      
