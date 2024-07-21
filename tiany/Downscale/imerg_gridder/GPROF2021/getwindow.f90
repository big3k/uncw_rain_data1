	SUBROUTINE GETWINDOW ( cjobid, atime0, atime1, nsec0, nsec1 )
!
!***********************************************************************
!* GETWINDOW	Computes processing window in seconds since 1 Jan 2000 *
!*								       *
!* Called by:	L2AGPROF2021_nngrid				       *
!*								       *
!* Calls:	T2SEC2K				       		       *
!*								       *
!* GETWINDOW computes the range of data to process, expressed in       *
!* seconds since 1 January 2000.				       *
!*								       *
!* Input parameters:						       *
!*  cjobid	CHAR	PPS job ID				       *
!*  atime0	CHAR    start time of processing window		       *
!*			( YYYYMMDD"T"HH:MM:SS )			       *
!*  atime1	CHAR	end time of processing window		       *
!*			( YYYYMMDD"T"HH:MM:SS )			       *
!*								       *
!*  Output parameters:						       *
!*  nsec0	INT	start time of window to process, in seconds    *
!*			since 1 January 2000			       *
!*  nsec1	INT	end time of window to process, in seconds      *
!*			since 1 January 2000			       *
!**								       *
!* Log:								       *
!* E.Nelkin/SSAI	09/13	First Version			       *
!* D.Bolvin/SSAI    	07/20   Replace GEMPAK routines		       *
!***********************************************************************
!
	IMPLICIT	NONE
!
	CHARACTER*17	:: atime0, atime1
	CHARACTER*256	:: cjobid
!
	INTEGER*4	:: itime0(6), itime1(6)
	INTEGER*4	:: nsec0, nsec1, iret
!
!***********************************************************************
!	Convert the character strings atime0, atime1 into integer
!	arrays itime0, itime1 containing year, month, day, hour,
!	minute, and second.
!
	CALL CHAR2INT ( cjobid,  atime0(1:4),   itime0(1) )
	CALL CHAR2INT ( cjobid,  atime0(5:6),   itime0(2) )
	CALL CHAR2INT ( cjobid,  atime0(7:8),   itime0(3) )
	CALL CHAR2INT ( cjobid,  atime0(10:11), itime0(4))
	CALL CHAR2INT ( cjobid,  atime0(13:14), itime0(5) )
	CALL CHAR2INT ( cjobid,  atime0(16:17), itime0(6) )
!
	CALL CHAR2INT ( cjobid,  atime1(1:4),   itime1(1) )
	CALL CHAR2INT ( cjobid,  atime1(5:6),   itime1(2) )
	CALL CHAR2INT ( cjobid,  atime1(7:8),   itime1(3) )
	CALL CHAR2INT ( cjobid,  atime1(10:11), itime1(4) )
	CALL CHAR2INT ( cjobid,  atime1(13:14), itime1(5) )
	CALL CHAR2INT ( cjobid,  atime1(16:17), itime1(6) )
!
!***********************************************************************
!	Convert the itime0, itime1 arrays into single values nsec0,
!	nsec1, representing seconds since 1 January 2000.
!
	CALL T2SEC2K(itime0, nsec0, iret)
	CALL T2SEC2K(itime1, nsec1, iret)
!
	RETURN
	END
