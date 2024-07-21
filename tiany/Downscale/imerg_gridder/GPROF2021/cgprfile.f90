	SUBROUTINE CGPRFILE (cjobid, gprfile, debug, grnh2021, iret)
!
!***********************************************************************
!* CGPRFILE	Closes the input GPROF2021 granule file		       *
!*								       *
!* Called by:	GETGPROFG					       *
!*   								       *
!* Calls:	TKclose				       		       *
!*  								       *
!* CGPRFILE closes the GPROF2021 (2AGPROF2021) data granule file.      *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  cjobid	CHAR    PPS job ID				       *
!*  gprfile	CHAR	name of the 2AGPROF2021 granule file	       *
!*  debug	LOGICAL flag indicating if data fields and messages    *
!*			should be output for debugging purposes	       *
!*  grnh2021	RECORD	structure containing information on the	       *
!*			2AGPROF2021 input data file		       *
!*  								       *
!* Output Parameters:						       *
!*								       *
!*  iret	INT	Return code: 0 = normal			       *
!*				    -1 = stop			       *
!*								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	02/02	First Version			       *
!* D.Bolvin/SSAI	08/09	Integrate PPS toolkit API	       *
!* D.Bolvin/SSAI	11/09	Add PPS job ID			       *
!* D.Bolvin/SSAI	06/11	Adapt API to GPM TK		       *
!* D.Bolvin/SSAI	08/13	Adapt from TMI			       *
!* E.Nelkin/SSAI	08/13	Eliminate TK_3IMERGmessages.h	       *
!* E.Nelkin/SSAI	09/13	Replace ABTERM_E with iret/RETURN      *
!* E.Nelkin/SSAI	01/14	Generic version - use instruments.h    *
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
	INTEGER*4	:: istatus, iret
!
	CHARACTER*256	:: cerroutn, cjobid
	CHARACTER*1024	:: gprfile
        LOGICAL         :: debug
!
!***********************************************************************
!	Close the 2AGPROF2021 file
!
	istatus = TKclose (grnh2021)
!
	IF  (istatus .NE. TK_SUCCESS) THEN
	    IF  (debug) THEN
	        WRITE (*,*) 'WARNING: UNABLE TO CLOSE FILE ', &
			    TRIM(gprfile), &
			    '; TERMINATING EXECUTION; SUBROUTINE ', &
			    'CGPRFILE'
	    END IF
!	    cerroutn = 'Error closing 2AGPROF file; subroutine cgprfile'
!	    CALL ABTERM_E (cjobid, cerroutn)
	    iret = -1
	    RETURN
	END IF
!
	RETURN
	END
