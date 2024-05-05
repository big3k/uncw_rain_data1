program testfov 

      implicit none

      ! for reprojection
      character (len=256) :: instrument 
      integer :: iret, i
      integer, parameter :: NFOV = 96
      REAL*4            :: w (0:NFOV+1), s (0:NFOV+1) 

      
      w=0.0
      s=0.0
      instrument = "ATMS"

      IF ( TRIM(instrument) .EQ. 'ATMS'  ) THEN
       write(*, *) "Doing ATMS"
      Else 
       stop
      End if 

      CALL FOV_DIM  ( NFOV, instrument, w, s, iret )

      if ( iret .ne. 0 ) then
        write(*, *) "FOV_DIM() failed" 
        stop
      end if 

      write(*, *)"<table><thead><tr><th>Scan id</th>"
      write(*, *)"<th>Cross-scan direction w (km)</th>"
      write(*, *)"<th>Along-scan direction s (km)</th></tr></thead>" 
      write(*, *)"<tbody>" 
      !Do i=0, NFOV+1
      Do i=1, NFOV
       write(*, *) "<tr><td>", i, "</td><td>", w(i), "</td><td>", s(i), "</td></tr>" 
      End do
      write(*, *)"</tbody></table>" 

       
end program testfov 
