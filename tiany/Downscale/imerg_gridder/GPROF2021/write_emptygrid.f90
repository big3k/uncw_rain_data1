	SUBROUTINE WRITE_EMPTYGRID(cjobid, empty_start, empty_stop, &
                           gran_num_6digit, LN, LT, iunit_g, &
			   grnh2021, outpath_g, instrument, &
			   algid, satid, debug, iret)
!
!***********************************************************************
!* WRITE_EMPTYGRID Writes a missing-filled grid in GPROF SSM/I format  *
!*								       *
!* Called by:	GETGPROFG					       *
!*   								       *
!* Calls:	MK_ONAME					       *
!*  								       *
!* WRITE_EMPTYGRID writes a missing-filled grid in the standard GPROF  *
!* SSM/I format if the input file was empty and the code is run in     *
!* "PPS mode".                                                         *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  cjobid	CHAR    PPS job ID				       *
!*  empty_start CHAR    metadata start date/time string                *
!*  empty_stop  CHAR    metadata stop date/time string                 *
!*  gran_num_6digit                                                    *
!*              CHAR    metadata six-digit granule number              *
!*  LN		INT	number of longitudinal grid boxes	       *
!*  LT		INT	number of latitudinal grid boxes	       *
!*  iunit_g	INT	unit number of granule_list_file, a	       *
!*			dynamically-built file containing start/end    *
!*			times, output filenames of all orbits processed*
!*  grnh2021	RECORD	structure containing information on the GPROF  *
!*			granule					       *
!*  outpath_g	CHAR	Path (directory) for gridded output, including *
!*			trailing slash (/)			       *
!*  instrument	CHAR	name of instrument (e.g., GMI, MHS)	       *
!*  algid	CHAR	algorithm id (e.g., GPROF2021_NNG)	       *
!*  satid	CHAR	name of satellite (e.g., GPM, NOAA19)	       *
!*  debug	LOGICAL flag indicating if data fields and messages    *
!*			should be output for debugging purposes	       *
!* 								       *
!* Output Parameters:						       *
!*								       *
!*  iret	INT	Return code: 0 = normal			       *
!*				    -4 = error opening output; return  *
!*                                  -5 = error writing output; return  *
!*                                  -6 = error closing output; return  *
!*                                  -7 = error writing iunit_g; return *
!* 								       *
!**								       *
!* Log:								       *
!* E.Nelkin/SSAI	03/20	First Version, based on GPROF2017OUT   *
!* D.Bolvin/SSAI    	07/20   Replace GEMPAK routines		       *
!* E.Nelkin/SSAI        08/20   ACCESS='STREAM' output, no guard bytes *
!* E.Nelkin/SSAI        05/21   GPROF2020 version                      *
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
	INTEGER*1	:: ncount(LT,LN), nambig(LT,LN), &
			   nrpix(LT,LN), igtime(LT,LN)
	INTEGER*2	:: isfcrain(LT,LN)
!
	INTEGER*4	:: start_time(6), end_time(6)
!
	INTEGER*4	:: i, j, iunit, iunit_g, iret
!!	INTEGER*4	:: IFILL1 = -99, IFILL2 = -9999
	INTEGER*1	:: IFILL1 = -99
	INTEGER*2	:: IFILL2 = -9999
!
        CHARACTER*50    :: empty_start, empty_stop
	CHARACTER*256	:: cjobid, cerroutn
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
!	Fill the arrays that will be written to the output file.
!
	DO j = 1, LN
	    DO i = 1, LT
		isfcrain(i,j) = IFILL2
		ncount(i,j) = IFILL1
		nambig(i,j) = IFILL1
		nrpix(i,j) = IFILL1
                igtime(i,j) = IFILL1
	    END DO
	END DO
!
!***********************************************************************
!	Assign the start_time and end_time arrays based on the metadata.
!       The metadata date/time comes in the form
!       YYYY-MM-DDTHH:MM:SS.000Z.
!
        CALL CHAR2INT( cjobid, empty_start(1:4),   start_time(1) )
        CALL CHAR2INT( cjobid, empty_start(6:7),   start_time(2) )
        CALL CHAR2INT( cjobid, empty_start(9:10),  start_time(3) )
        CALL CHAR2INT( cjobid, empty_start(12:13), start_time(4) )
        CALL CHAR2INT( cjobid, empty_start(15:16), start_time(5) )
        CALL CHAR2INT( cjobid, empty_start(18:19), start_time(6) )
!
        CALL CHAR2INT( cjobid, empty_stop(1:4),    end_time(1) )
        CALL CHAR2INT( cjobid, empty_stop(6:7),    end_time(2) )
        CALL CHAR2INT( cjobid, empty_stop(9:10),   end_time(3) )
        CALL CHAR2INT( cjobid, empty_stop(12:13),  end_time(4) )
        CALL CHAR2INT( cjobid, empty_stop(15:16),  end_time(5) )
        CALL CHAR2INT( cjobid, empty_stop(18:19),  end_time(6) )
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
