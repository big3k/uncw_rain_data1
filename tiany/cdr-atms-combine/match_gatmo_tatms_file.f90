
! give a gatmo file, find the matching tatms file, just in case, so we
! do not assume two kinds of files always come in pairs.  

      subroutine match_gatmo_tatms_file (tatms_matched_gatmo_file, &
                             gatmo_file, tatms_files, nf)
      implicit none
      INCLUDE "parms.h"
 
      integer :: nf, jf
      character(len=1064) :: gatmo_file
      character(len=1064) :: tatms_files(MAX_FILES_PER_DAY)
      character(len=1064) :: tatms_matched_gatmo_file

      character(len=6) :: stime1  ! start time of each file as string
      character(len=6) :: stime2  ! start time of each file as string
 
      stime1=gatmo_file(22:27) 
      tatms_matched_gatmo_file=""
      Do jf=1, nf
       stime2=tatms_files(jf)(22:27) 
       if ( stime1 == stime2 ) then 
           tatms_matched_gatmo_file = tatms_files(jf)
           return 
       end if 
      End Do 
      return 

      end 
       
        
      


