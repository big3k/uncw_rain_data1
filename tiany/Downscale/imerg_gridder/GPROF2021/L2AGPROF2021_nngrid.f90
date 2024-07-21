	PROGRAM L2AGPROF2021_NNGRID
!
!***********************************************************************
!* L2AGPROF2021_nngrid  	Grids the 2AGPROF2021 data	       *
!*								       *
!* This program reads and grids a granule of 2AGPROF2021.	       *
!*								       *
!***********************************************************************
!*     ***ATTENTION *** ATTENTION *** ATTENTION *** ATTENTION ***      *
!*								       *
!* This code is used for BOTH production and near-real-time (NRT).     *
!*								       *
!* PRODUCTION: Input files for all sensors are ORBITIZED, resulting in *
!* unique output grids.						       *
!*								       *
!* NRT: Input files are NOT ORBITIZED.  For many sensors, consecutive  *
!* files contain overlaps.  Occasionally, an input file may even       *
!* contain a complete subset of data from another file.  Since PPS     *
!* processes files "as is" upon arrival, files may be processed out of *
!* order.  In TMPA, this was controlled using auxiliary ".processed"   *
!* files.  In IMERG, since this single code must handle both production*
!* and NRT, we choose to simply let all data be gridded.  Downstream   *
!* IMERG code attempts to audit duplicate data. THE ARCHIVE OF NRT     *
!* GRIDDED FILES CONTAINS THESE OVERLAPS/DUPLICATES.		       *
!***********************************************************************
!*								       *
!* L2AGPROF2021_nngrid cinfile					       *
!*								       *
!* Input command-line arguments:				       *
!*								       *
!*   cinfile	CHAR	name of file containing command-line arguments *
!*								       *
!* cinfile is a simple ASCII file that contains the command-line       *
!* arguments for L2AGPROF2021_nngrid.  cinfile contains the following  *
!* arguments, one per line:					       *
!*								       *
!* Input command-line arguments:				       *
!*								       *
!*   cjobid	CHAR    PPS job ID				       *
!*   algid	CHAR	algorithm id (e.g., GPROF2021_NNG)	       *
!*   instrument CHAR	name of instrument (e.g., GMI, MHS)	       *
!*   satid	CHAR	name of satellite (e.g., GPM, NOAA19)	       *
!*   gprfile	CHAR    Input 2AGPROF2021 file name		       *
!*   start_time CHAR	Beginning of time window to process	       *
!*			(of the form YYYYMMDD"T"HH:MM:SS, w/o quotes)  *
!*   end_time	CHAR	End of time window to process		       *
!*			(of the form YYYYMMDD"T"HH:MM:SS, w/o quotes)  *
!*   granule_list_file						       *
!*		CHAR	dynamically-built file containing start/end    *
!*			times, output filenames of all orbits processed*
!*   outpath_g	CHAR	Path (directory) for gridded output, including *
!*			trailing slash (/)			       *
!*   cpps_mode  CHAR    flag indicating whether a corrupt input file   *
!*                      triggers a TKERROR ('T') or TKWARN ('F')       *
!*   cdebug	CHAR	flag indicating if data fields and messages    *
!*			should be output for debugging purposes	       *
!*			('T'=true, 'F'=false) [optional]	       *
!*   max_crravg CHAR    maximum allowable orbit-avg conditional rate   *
!*								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	08/13	First Version			       *
!* E.Nelkin/SSAI	08/13	Replace gmioutf with granule_list_file *
!*				on command line			       *
!* E.Nelkin/SSAI	08/13	Add outpath_g to command line	       *
!* E.Nelkin/SSAI	09/13	Add start/end_time to command line;    *
!*				call GETWINDOW and pass results to     *
!*				GETGMI				       *
!* E.Nelkin/SSAI	09/13	Allow for processing multiple files;   *
!*				open granule_list_file here and pass   *
!*				its unit number downstream	       *
!* E.Nelkin/SSAI	12/13	Generic version - read instrument name *
!*				to set NFOV, scans_per_orbit; pass name*
!*				to GETGPROFG			       *
!* E.Nelkin/SSAI	01/14	Add algid, satid, and pass to GETGPROFG*
!* E.Nelkin/SSAI	07/14	Increase scans_per_orbit for many      *
!*				instruments based on Joyce Chou 4/9/14 *
!*				e-mail; up her max nscan values by 3   *
!* E.Nelkin/SSAI	02/15	Expand gprfile, granule_list_file,     *
!*				glfile, outpath_g lengths to 1024      *
!* E.Nelkin/SSAI	02/15	Cut scans_per_orbit to 75% of previous *
!*				values to ensure grids do not lap      *
!*				themselves at high latitudes	       *
!* G.Huffman/612	05/16	Re-introduce Polar Tangent Plane scheme*
!*				for fully global coverage (from        *
!*				AMSU_GRID/FG_AVG, but nearest neighbor)*
!* E.Nelkin/SSAI	08/16	Add sensor-specific rate_thresh, set to*
!*				0.03 mm/h for GMI, zero otherwise      *
!* E.Nelkin/SSAI        02/17   GPM V04: revise rate_thresh values;    *
!*                              comment out MADRAS                     *
!* E.Nelkin/SSAI        04/17   Increase cerroutn to 256 + 1024 = 1280 *
!* E.Nelkin/SSAI        04/17   GPM V05: Set GMI rate_thresh back to   *
!*                              0.03 mm/h to best match V04            *
!* E.Nelkin/SSAI        04/17   V05: Set all rate_thresh to 0.03 for   *
!*                              now since no sounder data are available*
!* E.Nelkin/SSAI        07/17   Add cpps_mode and pass to GETGPROFG;   *
!*                              provides control to allow processing   *
!*                              beyond corrupt file if set to 'N'      *
!* E.Nelkin/SSAI        08/17   Add num_empty, num_outside, and logical*
!*                              wrote_output (PPS error control)       *
!* E.Nelkin/SSAI        05/18   Moved cdebug forward in argument list  *
!*                              in call to GETGPROFG                   *
!* E.Nelkin/SSAI        05/18   Read in crrmax and pass to GETGPROFG   *
!* D.Bolvin/SSAI    	07/20   Replace GEMPAK routines		       *
!* E.Nelkin/SSAI        05/21   Upgrade GPROF2017 code to GPROF2020    *
!* E.Nelkin/SSAI        06/21   Use logical pps_mode, debug variables  *
!* E.Nelkin/SSAI        01/22   Update GPROF2020 names to GPROF2021    *
!***********************************************************************
!
	IMPLICIT	NONE
#include "instruments.h"
!
!***********************************************************************
!	The latitude/longitude resolution and latitude offsets are
!	specified in parm_res.h.
!
	INCLUDE "parm_res.h"
!
	CHARACTER*1	:: cpps_mode, cdebug
	CHARACTER*1024	:: gprfile(30000)
	CHARACTER*1024	:: granule_list_file, glfile, outpath_g
	CHARACTER*256	:: cjobid, algid, satid, instrument
        CHARACTER*1280  :: cerroutn
	CHARACTER*17	:: atime0, atime1
        CHARACTER*10    :: max_crravg
	INTEGER*4	:: nsec0, nsec1
	INTEGER*4	:: NFOV, scans_per_orbit
	INTEGER*4	:: k, numfiles, iunit_g, num_empty, num_outside
        INTEGER*4       :: istatus, iret
	REAL*4		:: rate_thresh, crrmax
	LOGICAL		:: opn, wrote_output, debug, pps_mode
!
!***********************************************************************
!	Read the command-line arguments from input stream and return
!	them to L2AGPROF2021_nngrid for further processing.
!
	CALL READARGS (cjobid, algid, instrument, satid, gprfile, &
		       atime0, atime1, granule_list_file, outpath_g, &
		       cpps_mode, cdebug, max_crravg, numfiles, &
                       pps_mode, debug)
!
!***********************************************************************
!	Set NFOV and scans_per_orbit based on the instrument name.
!	Values obtained by adding 3 to the max scans reported by
!	Joyce Chou in 4/9/14 e-mail.  Assumptions are made that the
!	value for AMSRE is like AMSR2, AMSUB is like MHS, and SSMI is
!	like SSMIS.  The value for MADRAS came from John Stout in
!	12/19/13 e-mail.
!
!	2/11/2015: Scale the original values by 75%.  For sensors whose
!	orbit break is at high (northern) latitudes, the fanning out of
!	the grid boxes in an east-west direction caused overlap between
!	grid boxes at the end of the orbit and those from the beginning
!	of the orbit.  Reducing scans_per_orbit will prevent this, at
!	the expense of usually producing two output gridded files,
!	instead of one, per input file.
!
!	8/04/2016: Implement rate_thresh.  Rates below this value
!	will be ignored in ADDMSCAN.  Initially, set this to 0.03 mm/h
!	for GMI, based on an analysis by Chris Kidd of both POP and
!	precip rate thresholds by sensor and month.  Set the value to
!	zero for all other sensors, and let the IMERG calibration code
!	intercalibrate them against GMI.
!
!       2/15/2017: Revise the sensor-specific rate thresholds, based on
!       a recent analysis by Chris Kidd of 2015 data vs. MRMS (CONUS)
!       and European gauges, conducted by sensor and month.  The
!       resulting monthly thresholds were averaged by Dave Bolvin to
!       get a bulk value for each instrument.  The downstream IMERG
!       code will no longer perform intercalibration against GMI.
!       This is done to prevent large overestimates over land that
!       occurred with the previous approach.
!
!       4/21/2017: After experimentation with setting the GMI
!       threshold to 0.00 to test whether Erich Stocker's claim that
!       GPROF2017 goes back to pre-TMPAV7 in not producing rain
!       everywhere, it was determined that for continuity with V04,
!       V05 should retain the 0.03 mm/h GMI threshold.  In terms
!       of amount, continuity with V04 showed little difference
!       whether V05 used 0.03 or 0.00 mm/h.  In terms of frequency
!       of occurrence, the 0.03 mm/h threshold yielded better
!       continuity over land and low-latitude ocean.  Over the
!       Southern Ocean (July 2015), both threshold values yielded
!       persisently higher-than-V04 frequency of occurrence.
!
!       4/21/2017: For V05, set rate_thresh to 0.03 mm/hr for all
!       sensors for now, as this is our best guess since we have
!       not seen any V05 sounder data yet.  We will need to verify
!       that CSU has appropriately thresholded for us and may need
!       to adjust rate_thresh values in a subsequent delivery.  This
!       would be done by repeating the July 2015 GMI analysis
!       described in the previous paragraph for an MHS.
!
	IF ( TRIM(instrument) .EQ. 'AMSR2' ) THEN
	   NFOV = 486
	   scans_per_orbit = 2970
!!	   scans_per_orbit = 3960
	   rate_thresh = 0.03
!!         rate_thresh = 0.05
	ELSE IF ( TRIM(instrument) .EQ. 'AMSRE' ) THEN
	   NFOV = 392
	   scans_per_orbit = 2970
!!	   scans_per_orbit = 3960
	   rate_thresh = 0.03
!!         rate_thresh = 0.05
	ELSE IF ( TRIM(instrument) .EQ. 'AMSUB' ) THEN
	   NFOV = 90
	   scans_per_orbit = 1725
!!	   scans_per_orbit = 2300
	   rate_thresh = 0.03
!!         rate_thresh = 0.12
	ELSE IF ( TRIM(instrument) .EQ. 'ATMS' ) THEN
	   NFOV = 96
	   scans_per_orbit = 1716
!!	   scans_per_orbit = 2289
	   rate_thresh = 0.03
!!         rate_thresh = 0.12
	ELSE IF ( TRIM(instrument) .EQ. 'GMI' ) THEN
	   NFOV = 221
	   scans_per_orbit = 2224
!!	   scans_per_orbit = 2966
!!      rate_thresh = 0.00
           rate_thresh = 0.03
!!	ELSE IF ( TRIM(instrument) .EQ. 'MADRAS' ) THEN
!!	   NFOV = 960
!!	   scans_per_orbit = 1850
!!	   scans_per_orbit = 2467
!!	   rate_thresh = 0.
	ELSE IF ( TRIM(instrument) .EQ. 'MHS' ) THEN
	   NFOV = 90
	   scans_per_orbit = 1725
!!	   scans_per_orbit = 2300
	   rate_thresh = 0.03
!!         rate_thresh = 0.12
	ELSE IF ( TRIM(instrument) .EQ. 'SAPHIR' ) THEN
	   NFOV = 182
	   scans_per_orbit = 2803
!!	   scans_per_orbit = 3738
	   rate_thresh = 0.03
!!         rate_thresh = 0.12
	ELSE IF ( TRIM(instrument) .EQ. 'SSMI' ) THEN
! High-frequency (85 GHz) value:
	   NFOV = 128
	   scans_per_orbit = 2420
!!	   scans_per_orbit = 3226
	   rate_thresh = 0.03
!!         rate_thresh = 0.06
	ELSE IF ( TRIM(instrument) .EQ. 'SSMIS' ) THEN
	   NFOV = 180
	   scans_per_orbit = 2420
!!	   scans_per_orbit = 3226
	   rate_thresh = 0.03
!!         rate_thresh = 0.06
	ELSE IF ( TRIM(instrument) .EQ. 'TMI' ) THEN
	   NFOV = 208
	   scans_per_orbit = 2192
!!	   scans_per_orbit = 2924
	   rate_thresh = 0.03
	END IF
!
!***********************************************************************
!	Convert the input strings atime0, atime1 to integer arrays of
!	year/month/day/hour/minute/second, then return these as seconds
!	since 1 January 2000.  Only data within this window will be
!	processed.
!
	CALL GETWINDOW (cjobid, atime0, atime1, nsec0, nsec1)
!
!***********************************************************************
!       Convert the string max_crravg to real*4 crrmax, for use in
!       GETGPROFG as a quality-control measure of the maximum allowable
!       orbit-average conditional rate.
!
        CALL CHAR2REAL (cjobid, max_crravg, crrmax)
!
!***********************************************************************
!	Write the input parameters, if requested.
!
	IF  (debug) THEN
	    WRITE (*,*) 'jobid is ', TRIM(cjobid)
	    WRITE (*,*) 'algid is ', TRIM(algid)
	    WRITE (*,*) 'instrument is ', TRIM(instrument)
	    WRITE (*,*) 'satid is ', TRIM(satid)
	    WRITE (*,*) 'First GPROF file is ', TRIM(gprfile(1))
	    WRITE (*,*) 'Start time is ', TRIM(atime0)
	    WRITE (*,*) 'End time is ', TRIM(atime1)
	    WRITE (*,*) 'GPROF granule_list_file is ', &
			 TRIM(granule_list_file)
	    WRITE (*,*) 'Output path is ', TRIM(outpath_g)
            WRITE (*,*) 'PPS mode flag is ', pps_mode
	    WRITE (*,*) 'debug flag is ', debug
            WRITE (*,*) 'max orbit-avg crr is ', max_crravg
	END IF
!
!***********************************************************************
!	Open the granule_list_file and pass its unit number downstream.
!
	iunit_g = 0
	opn = .TRUE.
	DO WHILE (opn)
	   iunit_g = iunit_g + 1
	   INQUIRE (iunit_g, OPENED = opn)
	END DO
!
	glfile = TRIM(granule_list_file)
	OPEN(iunit_g, FILE = glfile, ACCESS = 'APPEND', &
	     STATUS = 'UNKNOWN', ERR = 103)
!
!***********************************************************************
!	Read and grid the 2AGPROF2021 data.
!
        num_empty = 0
        num_outside = 0
        wrote_output = .FALSE.
!
	DO k = 1, numfiles
	   CALL GETGPROFG (cjobid, LN, LT, LTS, ILATEXT, &
	   		 NCEDFRAC, NNSFRAC, NNS2CED, NNSX, NNSY, NFOV, &
			 gprfile(k), nsec0, nsec1, iunit_g, outpath_g, &
			 scans_per_orbit, instrument, rate_thresh, &
			 algid, satid, crrmax, pps_mode, debug, &
                         num_empty, num_outside, wrote_output )
	END DO
!
	CLOSE(iunit_g, ERR = 105)
	GO TO 201
!
  103	IF  (debug) THEN
	    WRITE (*,*) 'ERROR: UNABLE TO OPEN GRANULE LIST FILE', &
			'; TERMINATING EXECUTION'
	END IF
	cerroutn = 'Error opening granule_list_file; ' // &
		   'terminating execution'
	CALL ABTERM_E (cjobid, cerroutn)
!
  105	IF  (debug) THEN
	    WRITE (*,*) 'ERROR: UNABLE TO CLOSE GRANULE LIST FILE',&
			'; TERMINATING EXECUTION'
	END IF
	cerroutn = 'Error closing granule_list_file; ' // &
			'terminating execution'
	CALL ABTERM_E (cjobid, cerroutn)
!
!       PPS error control, as of August 8, 2017
!
!       Exit with success ("0") for the following conditions:
!
!       - At least one output file was written during the job (nominal)
!       - All input files were empty
!       - No output was written due to a *combination* of all input
!         files being empty or outside the requested time window
!
!       Exit with failure ("1") for the following condition:
!       - All input files were outside the requested time window
!
  201   IF ( wrote_output ) THEN
           CALL EXIT(0)
        ELSE
           IF ( num_empty .EQ. numfiles ) THEN
              CALL EXIT(0)
           ELSE IF ( num_outside .EQ. numfiles ) THEN
              cerroutn = 'ERROR: ALL FILES WERE OUTSIDE REQUESTED ' // &
                         'TIME RANGE'
              CALL ABTERM_E (cjobid, cerroutn)
           ELSE
              cerroutn = 'WARNING: COMBINATION OF EMPTY AND ' // &
                         'OUTSIDE-RANGE FILES RESULTED IN NO OUTPUT'
              istatus = TKmessage (cjobid, TKWARN, cerroutn)
              CALL EXIT(0)
           END IF
        END IF
	STOP
	END
