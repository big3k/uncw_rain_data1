	SUBROUTINE GPROF2021OUT (cjobid, LN, LT, iunit_g, gprglobe, &
			   grnh2021, outpath_g, iter, iscan0, &
			   instrument, scans_per_orbit, algid, satid, &
			   nscanf, gran_num_6digit, debug, iret)
!
!***********************************************************************
!* GPROF2021OUT	Dumps the GPROF global CED into the GPROF SSM/I format *
!*								       *
!* Called by:	GETGPROFG					       *
!*   								       *
!* Calls:	MK_ONAME					       *
!*  								       *
!* GPROF2021OUT dumps the global CED Level 3 mapping of GPROF orbit    *
!* data into the standard GPROF SSM/I format.			       *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  cjobid	CHAR	PPS job ID				       *
!*  LN		INT	number of longitudinal grid boxes	       *
!*  LT		INT	number of latitudinal grid boxes	       *
!*  iunit_g	INT	unit number of granule_list_file, a	       *
!*			dynamically-built file containing start/end    *
!*			times, output filenames of all orbits processed*
!*  gprglobe	REAL	Work array holding the global CED variables    *
!*  grnh2021	RECORD	structure containing information on the GPROF  *
!*			granule					       *
!*  outpath_g	CHAR	Path (directory) for gridded output, including *
!*			trailing slash (/)			       *
!*  iter	INT	iteration index for invocation of this routine *
!*  iscan0	INT	index of first scan of requested window	       *
!*  instrument	CHAR	name of instrument (e.g., GMI, MHS)	       *
!*  scans_per_orbit						       *
!*		INT	number of scans per orbit		       *
!*  algid	CHAR	algorithm id (e.g., GPROF2021_NNG)	       *
!*  satid	CHAR	name of satellite (e.g., GPM, NOAA19)	       *
!*  nscanf	INT	current position of scan pointer within orbit  *
!*  gran_num_6digit                                                    *
!*              CHAR    metadata six-digit granule number              *
!*  debug	LOGICAL flag indicating if data fields and messages    *
!*			should be output for debugging purposes	       *
!* 								       *
!* Output Parameters:						       *
!*								       *
!*  iret	INT	Return code: 0 = normal			       *
!*				    -1 = error backing up scan; return *
!*				    -2 = error reading scan; return    *
!*				    -3 = error rewinding ; return      *
!*				    -4 = error opening output; return  *
!*				    -5 = error writing output; return  *
!*				    -6 = error closing output; return  *
!*				    -7 = error writing iunit_g; return *
!*				    -8 = error resetting pointer;return*
!* 								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	05/02	First Version			       *
!* D.Bolvin/SSAI	08/09	Integrate PPS toolkit API	       *
!* D.Bolvin/SSAI	11/09	Add PPS job ID			       *
!* D.Bolvin/SSAI	06/11	Variable data resolution	       *
!* D.Bolvin/SSAI	06/11	Adapt API to GPM TK		       *
!* D.Bolvin/SSAI	08/13	Adapt from TMI			       *
!* E.Nelkin/SSAI	08/13	Create output filename (moutf); add    *
!*				granule_list_file and write to it      *
!* E.Nelkin/SSAI	08/13	Add outpath_g			       *
!* E.Nelkin/SSAI	08/13	Obtain end time of granule by backing  *
!*				up one scan; use iter to track # times *
!*				routine is invoked, for setting start/ *
!*				end times on extra-long input files    *
!* E.Nelkin/SSAI	09/13	Pass in iunit_g, not granule_list_file *
!* E.Nelkin/SSAI	09/13	Pass in ioffset to set start_time      *
!* E.Nelkin/SSAI	11/13	Split up gmitemp into gmitemp4 and     *
!*				gmitime, to ensure complete time field *
!* E.Nelkin/SSAI	12/13	Replace ioffset with iscan0	       *
!* E.Nelkin/SSAI	12/13	Mask out guard scans (fp = -98)	       *
!* E.Nelkin/SSAI	12/13	Generic version - use instruments.h,   *
!*				pass in instrument name, pass it to    *
!*				MK_ONAME			       *
!* E.Nelkin/SSAI	01/14	Include generic "sat.h"		       *
!* E.Nelkin/SSAI	01/14	Pass in algid, satid; pass to MK_ONAME *
!* E.Nelkin/SSAI	01/14	Convert output fields so (90N, 0E) at  *
!*				top left (N-S flip/translate 180 deg)  *
!* E.Nelkin/SSAI	01/14	Revise time test to allow igtime = 0   *
!* E.Nelkin/SSAI	07/14	Before exiting, set pointer back to    *
!*				position it held upon entry (= nscanf) *
!* E.Nelkin/SSAI	02/15	Expand outpath_g, moutf lengths to 1024*
!* G.Huffman/612	05/16	Pull out central CED-to-global CED     *
!*				shift and just hand in the global CED, *
!*				renamed gprglobe		       *
!* E.Nelkin/SSAI        01/17   Modify data_type values to PPS settings*
!* G.Huffman/612	06/17	Force all rates<0 to missing-filled,   *
!*				set IFILL1, IFILL2 to I1, I2	       *
!* E.Nelkin/SSAI        08/18   Modify data_type value for ATMS        *
!* E.Nelkin/SSAI        03/20   Pass in gran_num_6digit to write to    *
!*                              granule_list_file                      *
!* E.Nelkin/SSAI        07/20   ACCESS='STREAM' output, no guard bytes *
!* E.Nelkin/SSAI        06/21   Use logical debug variable             *
!* E.Nelkin/SSAI        01/22   Update GPROF2020 names to GPROF2021    *
!***********************************************************************
!
	IMPLICIT	NONE
!
#include "instruments.h"
!
	INTEGER*4	:: LN, LT
!
	INTEGER*1	:: ncountPPS(LT,LN), nambigPPS(LT,LN), &
			   nrpixPPS(LT,LN), igtimePPS(LT,LN)
	INTEGER*2	:: isfcrainPPS(LT,LN)
!
	INTEGER*1	:: ncount_NSflip(LT,LN), nambig_NSflip(LT,LN), &
			   nrpix_NSflip(LT,LN), igtime_NSflip(LT,LN)
	INTEGER*2	:: isfcrain_NSflip(LT,LN)
!
	INTEGER*1	:: ncount(LT,LN), nambig(LT,LN), &
			   nrpix(LT,LN), igtime(LT,LN)
	INTEGER*2	:: isfcrain(LT,LN)
!
	INTEGER*4	:: start_time(6), end_time(6)
!
	INTEGER*4	:: i, j, k, iunit, iunit_g, istatus, iret
!!	INTEGER*4	:: IFILL1 = -99, IFILL2 = -9999
	INTEGER*1	:: IFILL1 = -99
	INTEGER*2	:: IFILL2 = -9999
	INTEGER*4	:: iter, iscan0, ipos, scans_per_orbit, nscanf
!
	REAL*4		:: FILL = -99.0
	REAL*4		:: gprglobe(LT,LN,5)
!
	CHARACTER*256	:: cerroutn, cjobid
	CHARACTER*256	:: instrument, algid, satid
	CHARACTER*1024	:: outpath_g, moutf
	CHARACTER*24	:: data_type
	CHARACTER*8	:: bdate, edate
	CHARACTER*6	:: btime, etime, gran_num_6digit
!
	LOGICAL		:: opn, debug
!
	INCLUDE "sat.h"
!
!***********************************************************************
!	Fill the arrays that will be written to the output file.  Start
!	with arrays still in "PPS orientation", which have 90S at the
!	top and the dateline at the left edge.
!
!	Mask out the guard footprints for swath edges (to keep data from
!	spilling into surrounding no-data area) and start/stop (set up
!	to enforce correct summation of the counts field at orbit/segment
!	boundaries), plus any other missings (fp = -9998 [set in
!	ADDMSCAN, -98, -9999).  The last ELSE is left in case there are
!	non-negative cases with zero counts.
!
	DO j = 1, LN
	    DO i = 1, LT
		IF  ( gprglobe(i,j,1) .LT. 0. ) THEN
		    isfcrainPPS(i,j) = IFILL2
		    ncountPPS(i,j) = IFILL1
		    nambigPPS(i,j) = IFILL1
		    nrpixPPS(i,j) = IFILL1
		ELSE IF  ( gprglobe(i,j,2) .GT. 0.0 ) THEN
		    isfcrainPPS(i,j) = NINT (gprglobe(i,j,1) * 100.0)
		    ncountPPS(i,j) = INT (gprglobe(i,j,2))
		    nambigPPS(i,j) = INT (gprglobe(i,j,4))
		    nrpixPPS(i,j) = INT (gprglobe(i,j,3))
		ELSE
		    isfcrainPPS(i,j) = IFILL2
		    ncountPPS(i,j) = IFILL1
		    nambigPPS(i,j) = IFILL1
		    nrpixPPS(i,j) = IFILL1
		END IF
!
		IF ( gprglobe(i,j,5) .GE. 0.0 ) THEN
		   igtimePPS(i,j) = INT(gprglobe(i,j,5))
		ELSE
		   igtimePPS(i,j) = IFILL1
		END IF
	    END DO
	END DO
!
!***********************************************************************
!	Perform a north-south flip on the PPS arrays to put 90N at the
!	top.
!
	DO j = 1, LN
	    DO i = 1, LT
		isfcrain_NSflip((LT+1)-i, j) = isfcrainPPS(i,j)
		ncount_NSflip((LT+1)-i, j) = ncountPPS(i,j)
		nambig_NSflip((LT+1)-i, j) = nambigPPS(i,j)
		nrpix_NSflip((LT+1)-i, j) = nrpixPPS(i,j)
		igtime_NSflip((LT+1)-i, j) = igtimePPS(i,j)
	    END DO
	END DO
!
!***********************************************************************
!	Perform a 180-degree east-west translate, carefully handling
!	the circular boundary, to put 0E at the left.  Positions 1 to
!	LN/2 are reassigned to positions LN/2+1 to LN, and positions
!	LN/2+1 to LN are reassigned to positions 1 to LN/2.
!
	DO j = 1, LN
	    k = j + (LN/2)
	    IF ( k .GT. LN ) k = k - LN
	    DO i = 1, LT
		isfcrain(i,k) = isfcrain_NSflip(i,j)
		ncount(i,k) = ncount_NSflip(i,j)
		nambig(i,k) = nambig_NSflip(i,j)
		nrpix(i,k) = nrpix_NSflip(i,j)
		igtime(i,k) = igtime_NSflip(i,j)
	    END DO
	END DO
!
!***********************************************************************
!	Back up by one scan to get the end date/time of the orbit
!	segment to be output.
!
	istatus = TKseek (grnh2021, -1, TK_REL_SCAN_OFF)
!
	IF ( istatus .NE. TK_SUCCESS ) THEN
	   IF  ( debug ) &
	       WRITE (*,*) 'ERROR: UNABLE TO BACK UP BY ONE SCAN ', &
			   'WITHIN INPUT GPROF GRANULE, ', &
			   '; TERMINATING EXECUTION; ', &
			   'SUBROUTINE GPROF2021OUT'
!	   cerroutn = 'Error backing up one scan within input ' // &
!		      'GPROF granule; subroutine gprof2021out'
!	   CALL ABTERM_E(cjobid, cerroutn)
	   iret = -1
	   RETURN
	END IF
!
!***********************************************************************
!	Read the previous scan and get the date/time information.
!
	istatus = TKreadScan (grnh2021, scangprof)
!
	IF  ( istatus .NE. TK_SUCCESS ) THEN
	    IF  ( debug ) &
		WRITE (*,*) 'ERROR: UNABLE TO READ GPROF SCAN; ', &
			    'TERMINATING EXECUTION; SUBROUTINE ', &
			    'GPROF2021OUT'
!	    cerroutn = 'Error reading GPROF scan; ' // &
!		       'subroutine gprof2021out'
!	    CALL ABTERM_E(cjobid, cerroutn)
	    iret = -2
	    RETURN
	END IF
!
!***********************************************************************
!	Assign the end_time array.
!
	end_time(1) = scangprof.ScanTime.Year
	end_time(2) = scangprof.ScanTime.Month
	end_time(3) = scangprof.ScanTime.DayOfMonth
	end_time(4) = scangprof.ScanTime.Hour
	end_time(5) = scangprof.ScanTime.Minute
	end_time(6) = scangprof.ScanTime.Second
!
!***********************************************************************
!	Since the file pointer is at the end of the 2AGPROF granule,
!	reset it back to the beginning of the current "scans_per_orbit"-
!	scan block to get the first scan date/time info.
!
	ipos = (iter - 1)*scans_per_orbit+ iscan0
	istatus = TKseek (grnh2021, ipos, TK_ABS_SCAN_OFF)
!	istatus = TKseek (grnh2021, 0, TK_ABS_SCAN_OFF)
!
	IF  ( istatus .NE. TK_SUCCESS ) THEN
	    IF  ( debug ) THEN
		WRITE (*,*) 'ipos = ', ipos, ' & iter = ', iter
		WRITE (*,*) 'ERROR: UNABLE TO REWIND ', &
			    ' INPUT GPROF GRANULE, ', &
			    '; TERMINATING EXECUTION; ', &
			    'SUBROUTINE GPROF2021OUT'
	    END IF
!	    cerroutn = 'Error rewinding input GPROF granule; ' // &
!		       'subroutine gprof2021out'
!	    CALL ABTERM_E (cjobid, cerroutn)
	    iret = -3
	    RETURN
	END IF
!
!***********************************************************************
!	Read the first scan and get the date/time information.
!
	istatus = TKreadScan (grnh2021, scangprof)
!
	IF  ( istatus .NE. TK_SUCCESS ) THEN
	    IF  ( debug ) &
		WRITE (*,*) 'ERROR: UNABLE TO READ ', &
			    ' GPROF SCAN, ', &
			    '; TERMINATING EXECUTION; ', &
			    'SUBROUTINE GPROF2021OUT'
!	    cerroutn = 'Error reading GPROF scan; ' // &
!		       'subroutine gprof2021out'
!	    CALL ABTERM_E (cjobid, cerroutn)
	    iret = -2
	    RETURN
	END IF
!
!***********************************************************************
!	Assign the start_time array.
!
	start_time(1) = scangprof.ScanTime.Year
	start_time(2) = scangprof.ScanTime.Month
	start_time(3) = scangprof.ScanTime.DayOfMonth
	start_time(4) = scangprof.ScanTime.Hour
	start_time(5) = scangprof.ScanTime.Minute
	start_time(6) = scangprof.ScanTime.Second
!
!***********************************************************************
!	Write the data to the output file.
!
	iunit = 0
	opn = .TRUE.
!
	DO WHILE (opn)
	    iunit = iunit + 1
	    INQUIRE (iunit, OPENED = opn)
	END DO
!
	CALL MK_ONAME(outpath_g, algid, satid, instrument, start_time, &
		      end_time, moutf)
        OPEN (iunit, FILE = TRIM(moutf), FORM = 'UNFORMATTED', &
              STATUS = 'NEW', ACCESS = 'STREAM', ERR = 100)
!
	WRITE (iunit, ERR = 101) start_time
        WRITE (iunit, ERR = 101) igtime
        WRITE (iunit, ERR = 101) isfcrain
	WRITE (iunit, ERR = 101) ncount
	WRITE (iunit, ERR = 101) nambig
	WRITE (iunit, ERR = 101) nrpix
!
	CLOSE (iunit, ERR = 102)
!
!	Load values to be written to the granule list file.
!
        IF ( TRIM(instrument) .EQ. 'AMSR2' ) THEN
	   data_type = '2AGPROFGCOMW1AMSR2_GR   '
        ELSE IF ( TRIM(instrument) .EQ. 'AMSRE' ) THEN
	   data_type = '2AGPROFAQUAAMSRE_GR     '
        ELSE IF ( TRIM(instrument) .EQ. 'AMSUB' ) THEN
	   data_type = '2AGPROF' // TRIM(satid) // 'AMSUB_GR   '
        ELSE IF ( TRIM(instrument) .EQ. 'ATMS' ) THEN
           IF ( TRIM(satid) .EQ. 'NPP' ) THEN
	      data_type = '2AGPROF' // TRIM(satid) // 'ATMS_GR       '
           ELSE
! NOAA-20, NOAA-21, etc.:
              data_type = '2AGPROF' // TRIM(satid) // 'ATMS_GR    '
           END IF
        ELSE IF ( TRIM(instrument) .EQ. 'GMI' ) THEN
	   data_type = '2AGPROFGPMGMI_GR        '
!       ELSE IF ( TRIM(instrument) .EQ. 'MADRAS' ) THEN
!          data_type = 'MT_MADRAS_GRID    '
        ELSE IF ( TRIM(instrument) .EQ. 'MHS' ) THEN
	   data_type = '2AGPROF' // TRIM(satid) // 'MHS_GR     '
        ELSE IF ( TRIM(instrument) .EQ. 'SAPHIR' ) THEN
	   data_type = '2AGPROFMT1SAPHIR_GR     '
        ELSE IF ( TRIM(instrument) .EQ. 'SSMI' ) THEN
	   data_type = '2AGPROF' // TRIM(satid) // 'SSMI_GR       '
        ELSE IF ( TRIM(instrument) .EQ. 'SSMIS' ) THEN
	   data_type = '2AGPROF' // TRIM(satid) // 'SSMIS_GR      '
        ELSE IF ( TRIM(instrument) .EQ. 'TMI' ) THEN          
	   data_type = '2AGPROFTRMMTMI_GR       '
	END IF
!
	WRITE(bdate(1:4),'(i4.4)') start_time(1)
	WRITE(bdate(5:6),'(i2.2)') start_time(2)
	WRITE(bdate(7:8),'(i2.2)') start_time(3)
	WRITE(btime(1:2),'(i2.2)') start_time(4)
	WRITE(btime(3:4),'(i2.2)') start_time(5)
	WRITE(btime(5:6),'(i2.2)') start_time(6)
	WRITE(edate(1:4),'(i4.4)') end_time(1)
	WRITE(edate(5:6),'(i2.2)') end_time(2)
	WRITE(edate(7:8),'(i2.2)') end_time(3)
	WRITE(etime(1:2),'(i2.2)') end_time(4)
	WRITE(etime(3:4),'(i2.2)') end_time(5)
	WRITE(etime(5:6),'(i2.2)') end_time(6)
!
!	Write an entry to the granule_list_file.
!
	WRITE(iunit_g, 700, ERR = 104) &
                data_type, gran_num_6digit(1:6), &
		bdate(1:8), btime(1:2), btime(3:4), btime(5:6), &
		edate(1:8), etime(1:2), etime(3:4), etime(5:6), &
		TRIM(moutf)
  700	FORMAT(a24,2x,a6,2x,a8,'T',a2,':',a2,':',a2, &
	       2x,a8,'T',a2,':',a2,':',a2,2x,a)
!
!***********************************************************************
!	Re-set the pointer back to the position it held upon entry
!	to this subroutine.  The pointer had been moved back to the
!	beginning of the current "scans_per_orbit" block only to get
!	the start date/time information.
!
	ipos = nscanf
	istatus = TKseek (grnh2021, ipos, TK_ABS_SCAN_OFF)
	IF  ( istatus .NE. TK_SUCCESS ) THEN
	    IF  ( debug ) THEN
		WRITE (*,*) 'ipos = ', ipos, ' & iter = ', iter
		WRITE (*,*) 'ERROR: UNABLE TO RESET POINTER ', &
			    ' WITHIN INPUT GPROF GRANULE, ', &
			    '; TERMINATING EXECUTION; ', &
			    'SUBROUTINE GPROF2021OUT'
	    END IF
!           cerroutn = 'Error resetting pointer within input ' // &
!                      'GPROF granule; subroutine gprof2021out'
!           CALL ABTERM_E (cjobid, cerroutn)
	    iret = -8
	    RETURN
	END IF
!***********************************************************************
!
	GO TO 200
!
  100	IF  ( debug ) &
	    WRITE (*,*) 'ERROR: UNABLE TO OPEN GPROF GRIDDED FILE; ', &
			'TERMINATING EXECUTION; SUBROUTINE GPROF2021OUT'
!	cerroutn = 'Error opening GPROF gridded file; subroutine ' // &
!		   'gprof2021out'
!	CALL ABTERM_E (cjobid, cerroutn)
	iret = -4
	RETURN
!
  101   IF  ( debug ) &
	    WRITE (*,*) 'ERROR: UNABLE TO WRITE GPROF GRIDDED FILE; ', &
			'TERMINATING EXECUTION; SUBROUTINE GPROF2021OUT'
!	cerroutn = 'Error writing GPROF gridded file; subroutine ' // &
!		   'gprof2021out'
!	CALL ABTERM_E (cjobid, cerroutn)
	iret = -5
	RETURN
!
  102   IF  ( debug ) &
	    WRITE (*,*) 'ERROR: UNABLE TO CLOSE GPROF GRIDDED FILE; ', &
			'TERMINATING EXECUTION; SUBROUTINE GPROF2021OUT'
!	cerroutn = 'Error closing GPROF gridded file; subroutine ' // &
!		   'gprof2021out'
!	CALL ABTERM_E (cjobid, cerroutn)
	iret = -6
	RETURN
!
  104	IF  ( debug ) &
	    WRITE (*,*) 'ERROR: UNABLE TO WRITE GPROF GRANULE LIST ', &
			'FILE ; TERMINATING EXECUTION; ', &
			'SUBROUTINE GPROF2021OUT'
!	cerroutn = 'Error writing GPROF granule_list_file; ' // &
!			'subroutine gprof2021out'
!	CALL ABTERM_E (cjobid, cerroutn)
	iret = -7
	RETURN
!
  200	CONTINUE
!
	RETURN
	END
