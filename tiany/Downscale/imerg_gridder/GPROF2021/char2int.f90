	SUBROUTINE CHAR2INT (cjobid, cnumber, inumber)
!
!***********************************************************************
!* CHAR2INT	Converts a character string to an integer number       *
!*								       *
!* Calls:	ABTERM_E					       *
!*  								       *
!* Converts a character string to an integer number.  This is a        *
!* replacement for the GEMPAK subroutines ST_INTG and ST_NUMB. 	       *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  cjobid	CHAR    PPS job ID				       *
!*  cnumber	CHAR	integer number in character string format      *
!* 								       *
!* Output Parameters:						       *
!*								       *
!*  inumber	INT	integer number				       *
!* 								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	05/20	First version			       *
!***********************************************************************
	
	IMPLICIT	NONE
	
	INTEGER*4	:: inumber, ios
	
	CHARACTER*(*)	:: cnumber
	CHARACTER*256	:: cjobid, cerroutn
		
	READ (cnumber,*,IOSTAT=ios) inumber
			
	IF  (ios .NE. 0) THEN
	    cerroutn = 'ERROR: Unable to convert character string to &
	    		integer number'
!	    CALL ABTERM_E (cjobid, cerroutn)
	    WRITE (*,*) 'error'
	END IF
	
	RETURN
	END
	
