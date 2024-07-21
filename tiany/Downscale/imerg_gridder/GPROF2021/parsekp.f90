	SUBROUTINE PARSEKP (cjobid, cstring, keyword, parameter, &
			    lenk, lenp)
!
!***********************************************************************
!* PARSEKP	Parses the input keyword=parameter strings	       *
!*								       *
!* Calls:	ABTERM_E					       *
!*  								       *
!* Parses the input keyword=parameter strings based on separation      *
!* by '='.  This is a replacement for the GEMPAK subroutine ST_CLST.   *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  cjobid	CHAR    PPS job ID				       *
!*  cstring	CHAR	keyword=parameter string		       *
!* 								       *
!* Output Parameters:						       *
!*								       *
!*  keyword	CHAR	input keyword				       *
!*  parameter	CHAR	input parameter				       *
!*  lenk	INT	keyword length				       *
!*  lenp	INT	parameter length			       *
!* 								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	05/20	First version			       *
!***********************************************************************
	
	IMPLICIT	NONE
	
	INTEGER*4	:: ios, length, ires, lenk, lenp, ihold
	
	CHARACTER*(*)	:: cstring, keyword, parameter
	CHARACTER*256	:: cjobid, cerroutn
	
	length = LEN (TRIM (cstring))
	
!	Check that the length of the input string is > 0.
	
	IF  (length .EQ.0) THEN
	    cerroutn = 'ERROR: Keyword=parameter length is 0'
!	    CALL ABTERM_E (cjobid, cerroutn)
	WRITE (*,*) cerroutn
	END IF
	    	
	ires = INDEX (cstring(1:length), '=', BACK = .FALSE.)
	ihold = INDEX (cstring(1:length), '=', BACK = .TRUE.)
	
!	Check that '=' only occurs once.
	
	IF  (ires .NE. ihold) THEN
	    cerroutn = 'ERROR: More than one = in input &
	    		keyword=parameter string' 
	    WRITE (*,*) cerroutn
!	    CALL ABTERM_E (cjobid, cerroutn)
	END IF
	
	IF  (ires .EQ. 0) THEN
	    cerroutn = 'ERROR: Input keyword=parameter statement has &
			no = sign'
	    WRITE (*,*) cerroutn
!	    CALL ABTERM_E (cjobid, cerroutn)
	END IF
	
	keyword = cstring (1:ires - 1)
	lenk = SIZEOF (TRIM (keyword))
	
	IF  (lenk .EQ. 0) THEN
	    cerroutn = 'ERROR: Input keyword '//TRIM(keyword)//&
			' '//'is incorrect'
	    WRITE (*,*) cerroutn
!	    CALL ABTERM_E (cjobid, cerroutn)
	END IF
	
	parameter = cstring (ires + 1:length)
	lenp = SIZEOF (TRIM (parameter))
	
	IF  (lenp .EQ. 0) THEN
	    cerroutn = 'ERROR: Input parameter '//TRIM (parameter)// &
			' '//'is incorrect'
	    WRITE (*,*) cerroutn
!	    CALL ABTERM_E (cjobid, cerroutn)
	END IF
	
	RETURN
	END
