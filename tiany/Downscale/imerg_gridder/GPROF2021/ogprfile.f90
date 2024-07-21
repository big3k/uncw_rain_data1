	SUBROUTINE OGPRFILE (cjobid, gprfile, debug, instrument, &
			     grnh2021, iret)
!
!***********************************************************************
!* OGPRFILE	Opens the GPROF Level 2 granule file		       *
!*								       *
!* Called by:	GETGPROFG					       *
!*   								       *
!* Calls:	TKopen						       *
!*  								       *
!* OGPRFILE opens the 2AGPROF data granule file for eventual	       *
!* accumulation and gridding.				  	       *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  cjobid	CHAR    PPS job ID				       *
!*  gprfile	CHAR	name of the 2AGPROF file	 	       *
!*  debug	LOGICAL flag indicating if data fields and messages    *
!*			should be output for debugging purposes	       *
!*  instrument	CHAR	name of instrument (e.g., GMI, MHS)	       *
!* 								       *
!* Output Parameters:						       *
!*								       *
!*  grnh2021	RECORD	structure containing information on the        *
!*			2AGPROF data file			       *
!*  iret	INT	Return code: 0 = normal			       *
!*				    -1 = stop			       *
!*                                                                     *
!* 								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	02/02	First Version			       *
!* D.Bolvin/SSAI	08/09	Integrate PPS toolkit API	       *
!* D.Bolvin/SSAI	11/09	Add PPS job ID			       *
!* D.Bolvin/SSAI	06/11	Adapt API to GPM TK		       *
!* D.Bolvin/SSAI	08/13	Adapt from TMI			       *
!* E.Nelkin/SSAI	08/13	Eliminate TK_3IMERGmessages.h	       *
!* E.Nelkin/SSAI	09/13	Replace ABTERM_E with iret/RETURN      *
!* E.Nelkin/SSAI	01/14	Generic version - use instruments.h,   *
!*				pass in instrument name		       *
!* E.Nelkin/SSAI	02/15	Expand gprfile length to 1024	       *
!* E.Nelkin/SSAI        06/21   Use logical debug variable             *
!* E.Nelkin/SSAI        01/22   Update GPROF2020 names to GPROF2021    *
!***********************************************************************
!
	IMPLICIT	NONE
!
#include "instruments.h"
!
	INTEGER*4	:: istatus, iret
!
	CHARACTER*256	:: cerroutn, cjobid, instrument
	CHARACTER*1024	:: gprfile
        LOGICAL         :: debug
!
!***********************************************************************
!	Open the 2AGPROF file.  If there are any problems opening the
!	file, write an error message and terminate GETGPROFG execution.
!
	istatus = TKopen (gprfile, '2AGPROF'//TRIM(instrument), &
		  TKREAD, 'HDF5', cjobid, grnh2021, 1)
!
	IF  (istatus .NE. TK_SUCCESS) THEN
	    IF  (debug) THEN
		WRITE (*,*) 'ERROR: UNABLE TO OPEN FILE ', &
			    TRIM(gprfile), &
			    '; TERMINATING EXECUTION; SUBROUTINE ', &
			    'OGPRFILE'
	    END IF
!	    cerroutn = 'Error opening 2AGPROF file; subroutine ogprfile'
!	    CALL ABTERM_E (cjobid, cerroutn)
	    iret = -1
	    RETURN
	END IF
!
	RETURN
	END
