	SUBROUTINE T2SEC2K  ( itime, nsec, iret )
!***********************************************************************
!* T2SEC2K  Convert 6-integer date/time to seconds from 2000010100     *
!*								       *
!* This subroutine converts 6-integer time (YYYY,MM,DD,HH,MM,SS) to    *
!* seconds since the start of 1 Jan. 2000.  If a 2-digit YY is passed  *
!* in, values below 50 have 2000 added, and the rest have 1900 added.  *
!*								       *
!* T2SEC2K  ( itime, nsec, iret )				       *
!*								       *
!* Input parameters:						       *
!*   itime	INT4*	6-integer time: YYYY,MM,DD,HH,MM,SS	       *
!*								       *
!* Output parameters:						       *
!*   nsec	INT4	Corresponding number of seconds from the start *
!*			of 2000 (may be <0)			       *
!*   iret	INT	Return code: 0 = normal			       *
!*				    -1 = stop			       *
!**								       *
!* Log:								       *
!* G.Huffman/SSAI	02/04	Consolidate, update TIMS87.F, IDAYS.F  *
!***********************************************************************
	IMPLICIT	NONE
	INTEGER*4	:: itime (6), nsec, iret
	INTEGER*4	:: iymd,      jyyyy, jday, jsec, leap2000, &
			leapyyyy,  iday,  isec, nday, leapdex, i, &
			MONTH (12, 2)
	DATA		MONTH &
			/ 0,31,59,90,120,151,181,212,243,273,304,334, &
			  0,31,60,91,121,152,182,213,244,274,305,335 /
!
	iret = 0
!
!	Set the year, allowing for a 2-digit YY; values <50 are
!	assumed to be in the 21st century; the rest in the 20th.
!
	IF  ( itime (1) .LT. 50 )  THEN
	    jyyyy = itime (1) + 2000
	  ELSE IF  ( itime (1) .LT. 100 )  THEN
	    jyyyy = itime (1) + 1900
	  ELSE
	    jyyyy = itime (1)
	END IF
!
!	Observing leap year, get Julian day (day of year), then seconds
!	into the day.
!
	IF  ( MOD ( jyyyy, 4 ) .NE. 0 )  THEN
	    leapdex = 1
	  ELSE
	    leapdex = 2
	END IF
	jday = MONTH (itime (2), leapdex) + itime (3)
	jsec = itime (4) * 3600 + itime (5) * 60 + itime (6)
!
!	Set number of leap days in previous years for 2000 and the
!	input year.  Then compute the number of days from the start
!	of 2000 and convert to seconds and add the current day's
!	seconds.
!
	leap2000 = (2000  - 1) / 4 + 1
	leapyyyy = (jyyyy - 1) / 4 + 1
	nday     = (jyyyy - 2000) * 365 + leapyyyy - leap2000 + jday - 1
	nsec     = nday * 86400 + jsec
!
	RETURN
	END
