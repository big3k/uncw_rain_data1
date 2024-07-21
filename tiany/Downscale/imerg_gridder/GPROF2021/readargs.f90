	SUBROUTINE READARGS (farg1, farg2, farg3, farg4, farg5, farg6, &
			     farg7, farg8, farg9, farg10, farg11, &
                             farg12, ninfile, pps_mode, debug)
!
!***********************************************************************
!* READARGS  	Reads the input command line arguments		       *
!*								       *
!* Called by:	L2AGPROF2021_nngrid				       *
!*								       *
!* Calls:	ABTERM_E, GETARG, IARGC				       *
!*								       *
!* READARGS reads the file containing the command-line arguments,      *
!* supplied at the initiation of L2AGPROF2021_nngrid execution.  The   *
!* command-line arguments are extracted and parsed from the input file *
!* and passed to the appropriate subroutines.	 		       *
!*								       *
!* Input Parameters:						       *
!*								       *
!*  <none>							       *
!*								       *
!* Output Parameters:						       *
!*								       *
!*   farg1	CHAR    PPS job ID				       *
!*   farg2	CHAR	algorithm ID (e.g., GPROF2021_NNG)	       *
!*   farg3	CHAR	name of instrument (e.g., GMI, MHS)	       *
!*   farg4	CHAR	name of satellite (e.g., GPM, NOAA19)	       *
!*   farg5	CHAR	name of input 2AGPROF files; up to	       *
!*			15000 allowed; usually just 1 will run in RT   *
!*   farg6	CHAR	start time of processing window		       *
!*			( YYYYMMDD"T"HH:MM:SS )			       *
!*   farg7	CHAR	end time of processing window		       *
!*			( YYYYMMDD"T"HH:MM:SS )			       *
!*   farg8	CHAR	name of output granule_list_file	       *
!*   farg9	CHAR	name of the output directory		       *
!*   farg10     CHAR    flag indicating whether a corrupt input file   *
!*                      triggers a TKERROR ('T') or TKWARN ('F')       *
!*   farg11	CHAR	flag indicating if data fields and messages    *
!*			should be output for debugging purposes	       *
!*			('T'=true, 'F'=false) 			       *
!*   farg12     CHAR    maximum allowable orbit-avg conditional rate   *
!*   pps_mode   LOGICAL logical version of farg10                      *
!*   debug      LOGICAL logical version of farg11                      *
!*								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	08/13	First Version			       *
!* E.Nelkin/SSAI	08/13	Make farg3 granule_list_file; revise   *
!*				error codes			       *
!* E.Nelkin/SSAI	08/13	Make farg4 outpath_g		       *
!* E.Nelkin/SSAI	09/13	Add start_time, end_time as farg3,4    *
!* E.Nelkin/SSAI	09/13	Add ninfile for handling multiple files*
!* E.Nelkin/SSAI	12/13	Generic version - add instrument name  *
!*				as farg8, change "gmi" to gprof_infile *
!* E.Nelkin/SSAI	01/14	Add algid, satid; re-order arguments   *
!* E.Nelkin/SSAI	02/15	Expand farg5, farg8, farg9 lengths to  *
!*				1024				       *
!* E.Nelkin/SSAI        04/17   Increase cerroutn to 256 + 1024 = 1280 *
!* E.Nelkin/SSAI        07/17   Add farg10 (pps_mode); re-order args   *
!* E.Nelkin/SSAI        05/18   Add max_crr_avg as farg12, for QC      *
!* E.Nelkin/SSAI        06/18   Change farg12 default from 2 to 4 mm/hr*
!* E.Nelkin/SSAI        12/18   Change farg12 default to 5 mm/hr       *
!* D.Bolvin/SSAI    	07/20   Replace GEMPAK routines; dump invalid  *
!*				keywords 			       *
!* E.Nelkin/SSAI        05/21   GPROF2020 version                      *
!* E.Nelkin/SSAI        06/21   Use logical pps_mode, debug variables  *
!* E.Nelkin/SSAI        01/22   Update GPROF2020 names to GPROF2021    *
!***********************************************************************
!
	IMPLICIT	NONE
!
	INTEGER*4	:: IARGC, nargs, iunit, nin, iret, l_key, &
			   l_parm, i, ios, icount, ninfile
!
	CHARACTER*1	:: farg10, farg11, cdummy
	CHARACTER*256	:: farg1, farg2, farg3, farg4
	CHARACTER*1024	:: farg5(15000), farg8, farg9
	CHARACTER*17	:: farg6, farg7
        CHARACTER*10    :: farg12
	CHARACTER*256	:: cinfile, cstring, ckp(2)
        CHARACTER*1280  :: cerroutn
!
	LOGICAL		:: opn, pps_mode, debug
!
!***********************************************************************
!	Set the job ID to a dummy value in case it's needed before the
!	actual value is read.

	farg1 = &
	'L2AGPROF2021_nngrid_dummy_jobid -job failed before job ID read'
!
!***********************************************************************
!	Get the file name containing the command-line arguments.  If
!	there's an error, print an error message and terminate
!	L2AGPROF2021_nngrid execution.
!
	nargs = IARGC ()
!
	IF  (nargs .NE. 1) THEN
	    cerroutn = 'Error: incorrect number of command-line ' // &
		       'arguments; subroutine readargs'
	    CALL ABTERM_E (farg1, cerroutn)
	END IF
!
	CALL GETARG (1, cinfile)
!
!***********************************************************************
!	Open the ASCII file containing the L2AGPROF2021_nngrid command-
!	line arguments.  First, find an open file unit number.
!
	opn = .TRUE.
	iunit = 0
!
	DO WHILE (opn)
	    iunit = iunit + 1
	    INQUIRE (iunit, OPENED = opn)
	END DO
!
	OPEN (UNIT = iunit, FILE = cinfile, FORM = 'FORMATTED', &
	      ACCESS = 'SEQUENTIAL', ERR = 30, IOSTAT = ios)
!
!***********************************************************************
!	Determine the number of command-line arguments.  If there's a
!	debug flag, read it.  If not, set it to 'N'.  If there's an
!	error in the number of command-line arguments, print an error
!	message and terminate L2AGPROF2021_nngrid execution.
!
	icount = 0
!
  10	READ (iunit, 100, ERR = 30, END = 20) cdummy
	IF  (cdummy .NE. ' ') THEN
	    icount = icount + 1
	    GO TO 10
	END IF
!
  20	CONTINUE
!
	REWIND (iunit)
!
!***********************************************************************
!	Initialize and set some default values.
!
	iret = 0
	ninfile = 0
	farg2  = 'GPROF2021_NNG'
	farg3  = 'GMI'
	farg4  = 'GPM'
	farg6  = '19500101T00:00:00'
	farg7  = '20500101T23:59:59'
        farg10 = 'T'
	farg11 = 'F'
        farg12 = '5.00'
        pps_mode = .TRUE.
        debug = .FALSE.
!
!***********************************************************************
!	Read the input keyword=parameter arguments and parse
!	accordingly.
!
	DO i = 1, icount
!
	    READ (iunit, 101, ERR = 30) cstring
	    CALL PARSEKP (farg1, cstring, ckp(1), ckp(2), l_key, l_parm)

	    IF  (ckp(1)(1:l_key) .EQ. 'job_id') THEN
		farg1 = ckp(2)(1:l_parm)
	    ELSE IF  (ckp(1)(1:l_key) .EQ. 'alg_id') THEN
		farg2 = ckp(2)(1:l_parm)
	    ELSE IF  (ckp(1)(1:l_key) .EQ. 'instrument') THEN
		farg3 = ckp(2)(1:l_parm)
	    ELSE IF  (ckp(1)(1:l_key) .EQ. 'satellite') THEN
		farg4 = ckp(2)(1:l_parm)
	    ELSE IF  (ckp(1)(1:l_key) .EQ. 'gprof_infile') THEN
		IF (ninfile .GT. 15000) THEN
		   cerroutn = 'Error: more than 15000 file names; ' // &
			      'subroutine readargs'
		   CALL ABTERM_E(farg1, cerroutn)
		END IF
		ninfile = ninfile + 1
		farg5(ninfile) = ckp(2)(1:l_parm)
	    ELSE IF  (ckp(1)(1:l_key) .EQ. 'start_time') THEN
		farg6 = ckp(2)(1:l_parm)
	    ELSE IF  (ckp(1)(1:l_key) .EQ. 'end_time') THEN
		farg7 = ckp(2)(1:l_parm)
	    ELSE IF  (ckp(1)(1:l_key) .EQ. 'granule_list_file') THEN
		farg8 = ckp(2)(1:l_parm)
	    ELSE IF  (ckp(1)(1:l_key) .EQ. 'outdir') THEN
		farg9 = ckp(2)(1:l_parm)
            ELSE IF  (ckp(1)(1:l_key) .EQ. 'pps_mode') THEN
                farg10 = ckp(2)(1:l_parm)
	    ELSE IF  (ckp(1)(1:l_key) .EQ. 'debug_flag') THEN
		farg11 = ckp(2)(1:l_parm)
                IF ( farg11 .EQ. 'T' .OR. farg11 .EQ. 't' .OR. &
                     farg11 .EQ. 'Y' .OR. farg11 .EQ. 'y' ) THEN
                   debug = .TRUE.
                END IF
            ELSE IF  (ckp(1)(1:l_key) .EQ. 'max_crr_avg') THEN
                farg12 = ckp(2)(1:l_parm)
	    ELSE
            	cerroutn = 'ERROR: Invalid input keyword: ' // ckp(1)(1:l_key)
		CALL ABTERM_E (farg1, cerroutn)
	    END IF
!
	END DO
!
!       Ensure that pps_mode emerges from this routine as "T"
!       no matter what is entered, unless explicitly set to "F", "f", &
!       "N", or "n".
!
        IF ( farg10 .EQ. 'T' .OR. farg10 .EQ. 't' .OR. &
             farg10 .EQ. 'Y' .OR. farg10 .EQ. 'y' ) THEN
           pps_mode = .TRUE.
        ELSE IF ( farg10 .EQ. 'F' .OR. farg10 .EQ. 'f' .OR. &
                  farg10 .EQ. 'N' .OR. farg10 .EQ. 'n' ) THEN
           pps_mode = .FALSE.
        ELSE
           pps_mode = .TRUE.
        END IF
!
	GO TO 40
!
  30	cerroutn = 'Error opening/reading command-line file; ' // &
		   'subroutine readargs'
	CALL ABTERM_E (farg1, cerroutn)
!
  40	CONTINUE
!
	CLOSE (iunit)
!
 100	FORMAT (A1)
 101	FORMAT (A256)
!
	RETURN
	END
