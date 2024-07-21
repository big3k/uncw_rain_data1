	SUBROUTINE ABTERM_E (cjobid, cmsg)
!
!***********************************************************************
!* ABTERM_E	Controls the termination of L2AGPROF2021_NNGRID under  *
!*		abnormal circumstances				       *
!*								       *
!* Called by:	L2AGPROF2021_nngrid, READARGS, GETGPROFG	       *
!*   								       *
!* Calls:	TKmessage					       *
!*								       *
!* ABTERM_E calls TKmessage when any fatal problems are encountered    *
!* during 2AGPROF2021_NNGRID execution.  Nominal termination of        *
!* execution is handled within 2AGPROF2021_NNGRID.		       *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  cjobid	CHAR	PPS job ID				       *
!*  cmsg 	CHAR	execution error mnemonic for input to	       *
!*			TKmessage				       *
!*								       *
!* Output Parameters:						       *
!*								       *
!*  <none>							       *
!* 								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	02/02	First Version			       *
!* D.Bolvin/SSAI	08/09	Integrate PPS toolkit API	       *
!* D.Bolvin/SSAI	11/09	Add PPS job ID			       *
!* D.Bolvin/SSAI	06/11	Change API for GPM TK		       *
!* E.Nelkin/SSAI	09/13	Call exit(1)			       *
!* E.Nelkin/SSAI	01/14	Generic version - use instruments.h    *
!* E.Nelkin/SSAI        04/17   Increase cmsg to 256 + 1024 = 1280     *
!* E.Nelkin/SSAI        05/21   GPROF2020 version                      *
!* E.Nelkin/SSAI        01/22   Update GPROF2020 names to GPROF2021    *
!***********************************************************************
!
	IMPLICIT	NONE
!
#include "instruments.h"
!
	INTEGER*4	:: istatus
	CHARACTER*256	:: cjobid
        CHARACTER*1280  :: cmsg
!
!***********************************************************************
!	Call TKmessage and call it a day.
!
	istatus = TKmessage (cjobid, TKERROR, cmsg)
!
	CALL EXIT(1)
	STOP
	END
