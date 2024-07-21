	SUBROUTINE GETMSCAN (cjobid, grnh2021, scangprof, gprfile, &
			     debug, nsec0, nsec1, in_window, &
			     ieofflag, iret)
!
!***********************************************************************
!* GETMSCAN	Sequentially reads the GPROF scans		       *
!*								       *
!* Called by:	GETGPROFG					       *
!*   								       *
!* Calls:	T2SEC2K, TKreadScan				       *
!*  								       *
!* GETMSCAN retrieves a scan of GPROF data from the input GPROF granule*
!* file 2AGPROF.						       *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  cjobid	CHAR	PPS job ID				       *
!*  grnh2021	RECORD	structure containing information on the GPROF  *
!*			2AGPROF2021 input data file		       *
!*  scangprof	RECORD	structure containing GPROF scan information    *
!*  gprfile	CHAR	name of the GPROF granule file (2AGPROF)       *
!*  debug	LOGICAL flag indicating if data fields and messages    *
!*			should be output for debugging purposes	       *
!*  nsec0	INT	start time of window to process, in seconds    *
!*			since 1 January 2000			       *
!*  nsec1	INT	end time of window to process, in seconds      *
!*			since 1 January 2000			       *
!* 								       *
!* Output Parameters:						       *
!*								       *
!*  in_window	LOGICAL	flag indicating whether scan is within	       *
!*			requested time window			       *
!*  ieofflag	INT	flag indicating end of GPROF file or requested *
!*			window (0 != EOF, -1 = EOF)		       *
!*  iret	INT	Return code: 0 = normal			       *
!*				    -1 = stop			       *
!* 								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	02/02	First Version			       *
!* D.Bolvin/SSAI	08/09	Integrate PPS toolkit API	       *
!* D.Bolvin/SSAI	11/09	Add PPS job ID			       *
!* D.Bolvin/SSAI	06/11	Adapt API to GPM TK		       *
!* D.Bolvin/SSAI	08/13	Adapt from TMI			       *
!* E.Nelkin/SSAI	08/13	Eliminate TK_3IMERGmessages.h	       *
!* E.Nelkin/SSAI	09/13	Pass in nsec1 to cut off at end of     *
!*				requested time window, via ieofflag    *
!* E.Nelkin/SSAI	09/13	Replace ABTERM_E with iret/RETURN      *
!* E.Nelkin/SSAI	12/13	Pass in nsec0, return in_window	       *
!* E.Nelkin/SSAI	12/13	Generic version - use instruments.h    *
!* E.Nelkin/SSAI	01/14	Include generic "sat.h"		       *
!* E.Nelkin/SSAI	02/15	Expand gprfile length to 1024	       *
!* E.Nelkin/SSAI        05/21   GPROF2020 version                      *
!* E.Nelkin/SSAI        06/21   Use logical debug variable             *
!* E.Nelkin/SSAI        01/22   Update GPROF2020 names to GPROF2021    *
!***********************************************************************
!
	IMPLICIT	NONE
!
#include "instruments.h"
!
	INTEGER*4	:: istatus, ieofflag
	INTEGER*4	:: itime(6), nsec, nsec0, nsec1, iret
!
	CHARACTER*256	:: cerroutn, cjobid
	CHARACTER*1024	:: gprfile
!
	LOGICAL		:: in_window, debug
!
	INCLUDE "sat.h"
!
!***********************************************************************
!	Read the initial, or next, GPROF scan.  If the end of the GPROF
!	orbit is reached, set the ieofflag flag appropriately and return
!	control to GETGPROFG.  If there are any problems reading the
!	scan, write an error message and terminate GETGPROFG execution.
!
	IF  ( TKendOfFile (grnh2021) .NE. TK_EOF ) THEN
!
	    istatus = TKreadScan (grnh2021, scangprof)
!
	    IF  ( istatus .NE. TK_SUCCESS ) THEN
		IF  ( debug ) THEN
		    WRITE (*,*) 'ERROR: UNABLE TO READ NEXT SCAN OF ', &
				'DATA FROM FILE ', TRIM(gprfile), &
				'; TERMINATING EXECUTION; ', &
				'SUBROUTINE GETMSCAN'
		END IF
!		cerroutn = 'Error reading 2AGPROF scan; ' // &
!		'subroutine getmscan'
!		CALL ABTERM_E (cjobid, cerroutn)
		iret = -1
		RETURN
	    ELSE
!
!	If the current scan is beyond the end of the requested time
!	window, set the ieofflag appropriately, set the pointer back
!	one record to just before this first scan lying beyond the
!	requested time window, and return control to GETGPROFG.
!
		itime(1) = scangprof.scanTime.Year
		itime(2) = scangprof.scanTime.Month
		itime(3) = scangprof.scanTime.DayOfMonth
		itime(4) = scangprof.scanTime.Hour
		itime(5) = scangprof.scanTime.Minute
		itime(6) = scangprof.scanTime.Second
!
		CALL T2SEC2K(itime, nsec, iret)
		IF ( nsec .GE. nsec0 .AND. nsec .LE. nsec1 ) THEN
		   in_window = .TRUE.
		ELSE
		   in_window = .FALSE.
		END IF
!
		IF ( nsec .GT. nsec1 ) THEN
		   ieofflag = -1
		   istatus = TKseek (grnh2021, -1, TK_REL_SCAN_OFF)
		END IF
!
	    END IF
!
	ELSE
!
	    ieofflag = -1
!
	END IF
!
	RETURN
	END
