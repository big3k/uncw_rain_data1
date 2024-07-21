	SUBROUTINE CHAR2REAL (cjobid, cnumber, rnumber)
!
!***********************************************************************
!* CHAR2REAL	Converts a character string to a floating point number *
!*								       *
!* Calls:	ABTERM_E					       *
!*  								       *
!* Converts a character string to a floating point number.  This is a  *
!* replacement for the GEMPAK subroutine ST_CRNM. 		       *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  cjobid	CHAR    PPS job ID				       *
!*  cnumber	CHAR	real number in character string format	       *
!* 								       *
!* Output Parameters:						       *
!*								       *
!*  rnumber	REAL	real number				       *
!* 								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	05/20	First version			       *
!***********************************************************************

	IMPLICIT 	NONE
	
	INTEGER*4	:: ios
	
	REAL*4		:: rnumber
	
	CHARACTER*(*)	:: cnumber
	CHARACTER*256	:: cjobid, cerroutn
	
	READ (cnumber,*,IOSTAT=ios) rnumber
	
	IF  (ios .NE. 0) THEN
	    cerroutn = 'ERROR: Unable to convert character string to &
	    	        real number'
!	    CALL ABTERM_E (cjobid, cerroutn)
	    WRITE (*,*) 'error'
	END IF
	
	RETURN
	END
	
	
	
	
	
