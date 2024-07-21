	SUBROUTINE GETFST3 (cjobid, grnh2021, scangprof, gprfile, &
			    iter, nsec0, nsec1, iscan0, &
			    scans_per_orbit, debug, ifsmin, eogflag, &
			    iret)
!
!***********************************************************************
!* GETFST3	Gets the first scan time of the GPROF granule	       *
!*								       *
!* Called by:	GETGPROFG					       *
!*								       *
!* Calls:	TKreadScan, T2SEC2K				       *
!*  								       *
!* GETFST3 gets the time of the first scan in the GPROF granule.       *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  cjobid	CHAR	PPS job ID				       *
!*  grnh2021	RECORD	structure containing information on the 2AGPROF*
!*			input data file		       		       *
!*  scangprof	RECORD	structure containing GPROF scan information    *
!*  gprfile	CHAR	GPROF file name				       *
!*  iter	INT	iteration index for invocation of this routine *
!*  nsec0	INT	start time of window to process, in seconds    *
!*			since 1 January 2000			       *
!*  nsec1	INT	end time of window to process, in seconds      *
!*			since 1 January 2000			       *
!*  iscan0	INT	index of first scan of requested window	       *
!*  scans_per_orbit						       *
!*		INT	number of scans per orbit		       *
!*  debug	LOGICAL flag indicating if data fields and messages    *
!*			should be output for debugging purposes	       *
!*								       *
!* Output Parameters:						       *
!*								       *
!*  ifsmin	INT	time of first scan in minutes from beginning   *
!*			of day					       *
!*  eogflag	LOGICAL flag for end-of-granule, primarily to check if *
!*			first scan of block is beyond requested time   *
!*  iret	INT	Return code: 0 = normal			       *
!*				    -1 = Error resetting pointer;return*
!*				    -2 = Error reading GPF scan; return*
!*				    -3 = Bad initial scan time; return *
!* 								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	08/09	First Version			       *
!* D.Bolvin/SSAI	11/09	Add PPS job ID			       *
!* D.Bolvin/SSAI	06/11	Adapt API to GPM TK		       *
!* D.Bolvin/SSAI	08/13	Adapt from TMI			       *
!* E.Nelkin/SSAI	08/13	Eliminate TK_3IMERGmessages.h	       *
!* E.Nelkin/SSAI	09/13	Use iter to track # times routine is   *
!*				invoked, for setting start/end times   *
!*				on extra-long input files	       *
!* E.Nelkin/SSAI	09/13	Process only the range [nsec0, nsec1]  *
!* E.Nelkin/SSAI	09/13	icount controls end-of-file gracefully *
!* E.Nelkin/SSAI	09/13	Replace ABTERM_E with iret/RETURN      *
!* E.Nelkin/SSAI	12/13	Replace ioffset with iscan0	       *
!* E.Nelkin/SSAI	12/13	Generic version - use instruments.h    *
!* E.Nelkin/SSAI	01/14	Include generic "sat.h"		       *
!* E.Nelkin/SSAI	02/15	Expand gprfile length to 1024	       *
!* E.Nelkin/SSAI        06/21   Use logical debug variable             *
!* E.Nelkin/SSAI        01/22   Update GPROF2020 names to GPROF2021    *
!***********************************************************************
!
	IMPLICIT	NONE
!
#include "instruments.h"
!
	INTEGER*4	:: istatus, iter, ipos, iscan0, icount, ifsmin
	INTEGER*4	:: nsec0, nsec1, nsec, scans_per_orbit, iret
	INTEGER*4	:: itime(6)
!
	LOGICAL		:: eogflag, debug
!
	CHARACTER*256	:: cerroutn, cjobid
	CHARACTER*1024	:: gprfile
!
	INCLUDE "sat.h"
!
!***********************************************************************
!	Position the scan file pointer to the beginning of the first
!	scan in the current "scans per orbit"-scan block to begin
!	accumulating data.
!
	icount = 1
   30	ipos = (iter - 1)*scans_per_orbit+ iscan0
	istatus = TKseek (grnh2021, ipos, TK_ABS_SCAN_OFF)
!	istatus = TKseek (grnh2021, 0, TK_ABS_SCAN_OFF)
!
	IF  (istatus .NE. TK_SUCCESS) THEN
	    IF  (iter .GT. 1 .AND. icount .EQ. 1) eogflag = .TRUE.
	    IF  (debug) &
		WRITE (*,*) 'ERROR: UNABLE TO RESET FILE POINTER IN ', &
			    TRIM(gprfile), '; TERMINATING EXECUTION; ',&
			    'SUBROUTINE GETFST3'
!	    cerroutn = 'Error resetting 2AGPROF file pointer; ' // &
!		       'subroutine getfst3'
!	    CALL ABTERM_E (cjobid, cerroutn)
	    iret = -1
	    RETURN
	END IF
!
!***********************************************************************
!	Read the initial GPROF scan of the current "scans per orbit"-
!	scan block.  If there are any problems reading the scan, write
!	an error message and terminate GETFST3 execution.
!
	istatus = TKreadScan (grnh2021, scangprof)
!
	IF  (istatus .NE. TK_SUCCESS) THEN
	    IF  (debug) &
		WRITE (*,*) 'ERROR: UNABLE TO READ NEXT SCAN ', &
			    'OF DATA FROM FILE ', TRIM(gprfile), &
			    '; TERMINATING EXECUTION; ', &
			    'SUBROUTINE GETFST3'
!	    cerroutn = 'Error reading 2AGPROF scan; subroutine getfst3'
!	    CALL ABTERM_E (cjobid, cerroutn)
	    iret = -2
	    RETURN
	END IF
!
!***********************************************************************
!	Load the array itime, then convert the current scan to seconds
!	since 1 January 2000, to determine whether the scan is within
!	the requested processing window.
!
	itime(1) = scangprof.scanTime.Year
	itime(2) = scangprof.scanTime.Month
	itime(3) = scangprof.scanTime.DayOfMonth
	itime(4) = scangprof.scanTime.Hour
	itime(5) = scangprof.scanTime.Minute
	itime(6) = scangprof.scanTime.Second
!
	CALL T2SEC2K(itime, nsec, iret)
!
!***********************************************************************
!	If the first scan of the current "scans per orbit" -scan block
!	is later than the requested time window, set eogflag to .TRUE.
!	and return.  The calling program will then close the file and
!	exit.
!
	IF (nsec .GT. nsec1) THEN
	   eogflag = .TRUE.
	   RETURN
	END IF
!
!***********************************************************************
!	Set the first scan time in the granule and make sure it's good.
!
	ifsmin = (60 * (scangprof.scanTime.Hour)) + &
		  scangprof.scanTime.Minute + &
		  NINT (FLOAT (scangprof.scanTime.Second) / 60.0)
!
	IF  (ifsmin .LT. 0) THEN
	    IF  (debug) &
		WRITE (*,*) 'ERROR: BAD INITIAL SCAN TIME FROM FILE ', &
			    TRIM(gprfile), '; TERMINATING EXECUTION; ',&
			    'SUBROUTINE GETFST3'
!	    cerroutn = 'Error: Bad initial 2AGPROF scan time; ' // &
!		       'subroutine getfst3'
!	    CALL ABTERM_E (cjobid, cerroutn)
	    iret = -3
	    RETURN
	END IF
!
!***********************************************************************
!	Back up by one scan before exiting this routine, in order to
!	be positioned correctly when the calling program invokes
!	GETMSCAN.
!
	istatus = TKseek (grnh2021, -1, TK_REL_SCAN_OFF)
!
	IF  (istatus .NE. TK_SUCCESS) THEN
	    IF  (debug) &
		WRITE (*,*) 'ERROR: UNABLE TO RESET FILE POINTER IN ', &
			    TRIM(gprfile), '; TERMINATING EXECUTION; ',&
			    'SUBROUTINE GETFST3'
!	    cerroutn = 'Error resetting 2AGPROF file pointer; ' // &
!		       'subroutine getfst3'
!	    CALL ABTERM_E (cjobid, cerroutn)
	    iret = -1
	    RETURN
	END IF
!
	RETURN
	END
