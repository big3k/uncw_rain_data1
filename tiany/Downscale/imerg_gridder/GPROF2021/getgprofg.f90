	SUBROUTINE GETGPROFG ( cjobid, LN, LT, LTS, ILATEXT, NCEDFRAC, &
			     NNSFRAC, NNS2CED, NNSX, NNSY, NFOV, &
			     cgfile, nsec0, nsec1, iunit_g, outpath_g, &
			     scans_per_orbit, instrument, rate_thresh, &
			     algid, satid, crrmax, pps_mode, debug, &
                             num_empty, num_outside, wrote_output )
!
!***********************************************************************
!* GETGPROFG	Computes and outputs a granule average of GPROF data   *
!*								       *
!* Called by:	L2AGPROF2021_nngrid				       *
!*   								       *
!* Calls:	FOV_DIM, OGPRFILE, GETMSCAN, GETFST3, ADDMSCAN,	       *
!*		FOV_ORC, FOV_ORNS, FOV2GC_TIMEONLY, FOV2GNS_TIMEONLY,  *
!*		FOV2GC,  FOV2GNS, ZONES2GLOBE, GPROF2021OUT, CGPRFILE, *
!*              ABTERM_E                                               *
!*  								       *
!* GETGPROFG computes the average for a granule of 2AGPROF data.       *
!*  								       *
!* Input parameters:						       *
!*  cjobid	CHAR	PPS job ID				       *
!*  LN		INT	number of longitudinal grid boxes	       *
!*  LT		INT	number of latitudinal grid boxes	       *
!*  LTS		INT	number of latitudinal grid boxes in central CED*
!*  ILATEXT     INT	starting latitude of data extent in degrees    *
!*		  	(assume symmetrical)			       *
!*  NCEDFRAC    INT     NS grid size as gridboxes per degree           *
!*  NNSFRAC     INT     NS grid size as gridboxes per degree           *
!*  NNS2CED     INT     NS grid increments in a CED grid increment     *
!*  NNSX	INT	X size of north and south PTP		       *
!*  NNSY	INT	Y size of north and south PTP		       *
!*  NFOV	INT	number of fields-of-view per scan	       *
!*  cgfile	CHAR	name of the GPROF granule file (2AGPROF2021)   *
!*  nsec0	INT	start time of window to process, in seconds    *
!*			since 1 January 2000			       *
!*  nsec1	INT	end time of window to process, in seconds      *
!*			since 1 January 2000			       *
!*  iunit_g	INT	unit number of granule_list_file, a	       *
!*			dynamically-built file containing start/end    *
!*			times, output filenames of all orbits processed*
!*  outpath_g	CHAR	Path (directory) for gridded output, including *
!*			trailing slash (/)			       *
!*  scans_per_orbit						       *
!*		INT	number of scans per orbit		       *
!*  instrument	CHAR	name of instrument (e.g., GMI, MHS)	       *
!*  rate_thresh	REAL	precipitation rate threshold (mm/h)	       *
!*  algid	CHAR	algorithm id (e.g., GPROF2021_NNG)	       *
!*  satid	CHAR	name of satellite (e.g., GPM, NOAA19)	       *
!*  crrmax      REAL    maximum orbit-average conditional rate allowed,*
!*                      to weed out rare bad input data                *
!*  pps_mode    LOGICAL flag indicating whether a corrupt input file   *
!*                      triggers a TKERROR ('T') or TKWARN ('F')       *
!*  debug       LOGICAL flag indicating if data fields and messages    *
!*                      should be output for debugging purposes        *
!*                                                                     *
!* Output parameters:                                                  *
!*                                                                     *
!*  num_empty   INT     number of empty files during this run          *
!*  num_outside INT     number of files during this run completely     *
!*                      outside the requested time range               *
!*  wrote_output                                                       *
!*              LOGICAL flag indicating whether ANY output has been    *
!*                      written during this job (starts F, becomes T)  *
!*                                                                     *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	02/02	First Version			       *
!* D.Bolvin/SSAI	05/02	Output gridded GMI data		       *
!* D.Bolvin/SSAI	02/03	Add ambiguous screening after output   *
!*				of data; add screening on fraction of  *
!*				ambiguous			       *
!* D.Bolvin/SSAI	08/09	Integrate PPS toolkit API	       *
!* D.Bolvin/SSAI	11/09	Add PPS job ID			       *
!* D.Bolvin/SSAI	06/11	Variable data resolution	       *
!* D.Bolvin/SSAI	06/11	Adapt API to GPM TK		       *
!* G.Huffman/612	06/13	Adapt GETGMIG; GMIGAVG isn't needed in *
!*				nearest neighbor		       *
!* D.Bolvin/SSAI	08/13	Adapt to RT			       *
!* E.Nelkin/SSAI	08/13	Replace moutf with granule_list_file   *
!* E.Nelkin/SSAI	08/13	Add outpath_g to argument list	       *
!* E.Nelkin/SSAI	09/13	Break up output into multiple files of *
!*				2890 scans, as needed		       *
!* E.Nelkin/SSAI	09/13	Process only data within [nsec0,nsec1] *
!* E.Nelkin/SSAI	09/13	Pass in iunit_g, not granule_list_file *
!* E.Nelkin/SSAI	09/13	Pass ioffset to GMIOUT		       *
!* E.Nelkin/SSAI	09/13	Add iret to various subroutine calls;  *
!*				return to main if error encountered    *
!* E.Nelkin/SSAI	11/13	Split up gmitemp into gmitemp4 and     *
!*				gmitime, to ensure complete time field *
!* E.Nelkin/SSAI	12/13	Add code to handle bogus scan lines;   *
!*				rename ioffset "iscan0"; add iscan1;   *
!*				read entire file, rewind, then process *
!* E.Nelkin/SSAI	12/13	Generic version - use instruments.h,   *
!*				pass in NFOV, scans_per_orbit,	       *
!*				instrument			       *
!* E.Nelkin/SSAI	01/14	Include generic "sat.h"		       *
!* E.Nelkin/SSAI	01/14	Pass NFOV to FOV_DIM, FOV_OR, ADDMSCAN *
!* E.Nelkin/SSAI	01/14	Pass instrument to FOV_DIM	       *
!* E.Nelkin/SSAI	01/14	Pass in algid, satid, and pass these to*
!*				GPROF2014OUT			       *
!* E.Nelkin/SSAI	01/14	Initialize gprtime to -1, since 0 is a *
!*				valid value			       *
!* E.Nelkin/SSAI	07/14	Add test to prevent looping back up    *
!				when positioned at EOF marker	       *
!* E.Nelkin/SSAI	07/14	Pass nscanf to GPROF2014OUT	       *
!* E.Nelkin/SSAI	07/14	Add check for empty file (containing   *
!*				metadata only, but no data scans)      *
!* E.Nelkin/SSAI	07/14	Close input file if any error occurs   *
!* E.Nelkin/SSAI	10/14	Protect iscan0, iscan1 computations    *
!*				when very few scans fall in window     *
!* E.Nelkin/SSAI	10/14	Modify message when input file lies    *
!*				entirely outside requested window      *
!* E.Nelkin/SSAI	10/14	Toolkit bug - "empty" not cleared; test*
!*				on just first 5 characters of string   *
!* E.Nelkin/SSAI	02/15	Expand cgfile, outpath_g lengths to    *
!*				1024				       *
!* E.Nelkin/SSAI	02/15	Up do loop limit to 8*scans_per_orbit; *
!*				counteracts decrease of scans_per_orbit*
!*				definition in L2AGPROF2014_nngrid.f90  *
!* G.Huffman/612	05/16	Pull the shift of the central CED-to-  *
!*				global CED from GPROF2014OUT to        *
!*				ZONES2GLOBE, which also gets the N and *
!*				S PTP-to-global CED transformations    *
!* E.Nelkin/SSAI	08/16	Add sensor-specific rate_thresh, passed*
!*				in from main code and out to ADDMSCAN  *
!* E.Nelkin/SSAI        04/17   Exit (ABTERM_E) upon error, rather than*
!*                              continuing to next file in input list  *
!* E.Nelkin/SSAI        04/17   Increase cerroutn to 256 + 1024 = 1280 *
!* G.Huffman/612	05/17	Edge-guard footprints		       *
!* G.Huffman/612	06/17	Add instrument to ADDMSCAN	       *
!* E.Nelkin/SSAI        07/17   Add cpps_mode option to process beyond *
!*                              corrupt file if set to 'N'             *
!* E.Nelkin/SSAI        07/17   Set EMPTY case back to warn-and-advance*
!* E.Nelkin/SSAI        08/17   File outside requested window now warn-*
!*                              and-advance; track num_empty,          *
!*                              num_outside, and wrote_output          *
!* E.Nelkin/SSAI        09/17   Read ProductVersion, set NOUTER, passed*
!*                              to ADDMSCAN for use with MHS/ATMS      *
!* E.Nelkin/SSAI        09/17   Define/use RLATEXT as FLOAT(ILATEXT)   *
!* E.Nelkin/SSAI        09/17   Offset edges by NOUTER (MHS/ATMS)      *
!* E.Nelkin/SSAI        09/17   Change imin0 to -1 to avoid assigning  *
!*                              zero minutes to time of guard scans    *
!* E.Nelkin/SSAI        05/18   Moved cdebug forward in argument list  *
!* E.Nelkin/SSAI        05/18   Add crrmax for QC'ing out anomalous    *
!*                              cases of widespread high precip rates  *
!* E.Nelkin/SSAI        09/18   Add NOUTER=5 for AMSU-B                *
!* E.Nelkin/SSAI        10/18   Mask out AMSR2 snow-covered surfaces   *
!* E.Nelkin/SSAI        02/20   Write an empty grid file if empty input*
!*                              encountered and in PPS mode            *
!* E.Nelkin/SSAI        03/20   Read metadata granule number, start/   *
!*                              stop date/times after opening file;    *
!*                              pass granule number to GPROF2170OUT    *
!* D.Bolvin/SSAI    	07/20   Replace GEMPAK routines		       *
!* E.Nelkin/SSAI        08/20   Add FILLCHK to avoid math with missing *
!*                              values; change FILL to -9999.9         *
!* E.Nelkin/SSAI        05/21   GPROF2020 version                      * 
!* E.Nelkin/SSAI        06/21   Use logical pps_mode, debug variables  *
!* E.Nelkin/SSAI        01/22   Update GPROF2020 names to GPROF2021    *
!***********************************************************************
!
	IMPLICIT	NONE
!
!***********************************************************************
!
!       gprtemp4(lat,lon,parms)
!
!         where   lat:          0.1 degree latitude box
!		  lon:          0.1 degree longitude box
!                 parms:        1 = average precipitation rate
!                               2 = total # obs
!				3 = # raining obs
!				4 = # ambiguous obs
!	gprtime(lat,lon)
!				5 = scan time in minutes since start
!				    of granule
!	gprtemp4n, gprtimen, gprtemp4s, gprtimes are the same, but for
!	the PTP with a 0.025 deg grid.
!
!	influ (lon,lat) has the opposite lat/lon ordering.
!
#include "instruments.h"
!
	INTEGER*4	:: LN, LT, LTS, ILATEXT, NCEDFRAC, NNSFRAC, &
			   NNS2CED, NNSX, NNSY, NFOV, scans_per_orbit
	INTEGER*4	:: i, j, k, ieofflag, iret, istatus, ifsmin
	INTEGER*4	:: m, n
	INTEGER*4	:: nsec0, nsec1, iscan0, iscan1, num_empty, &
                           num_outside, NOUTER
        INTEGER*4       :: igran_numb
!
	REAL*4		:: FILL, FILLCHK, RLATEXT
	PARAMETER	( FILL    = -9999.9, FILLCHK = -8888. )
!
	REAL*4		:: gprtemp4c(LTS,LN,4), w (0:NFOV+1), s (0:NFOV+1)
	REAL*4		:: gprtimec(LTS,LN)
	REAL*4		:: gprtemp4n(NNSX,NNSY,4), gprtimen(NNSX,NNSY),&
			   gprtemp4s(NNSX,NNSY,4), gprtimes(NNSX,NNSY)
	REAL*4		:: gprglobe(LT,LN,5)
	INTEGER*1	:: influc (LN, LTS), influ_timec (LN, LTS), &
			   influn (NNSX, NNSY), influs (NNSX, NNSY), &
			   influ_timen (NNSX, NNSY), &
			   influ_times (NNSX, NNSY)
	REAL*4		:: flat(0:NFOV+1, 8*scans_per_orbit), &
			   flon(0:NFOV+1, 8*scans_per_orbit)
	REAL*4		:: flatb(0:NFOV+1, 2), flonb(0:NFOV+1, 2)
	REAL*4		:: fpb(0:NFOV+1, 2), ftb(0:NFOV+1, 2)
	INTEGER*4	:: jtime(8*scans_per_orbit)
	REAL*4		:: ambig, xorc(0:NFOV+1), yorc(0:NFOV+1), &
			   xorns(0:NFOV+1), yorns(0:NFOV+1)
	REAL*4		:: rate_thresh, diff, crrmax
!
	CHARACTER*256	:: cjobid
	CHARACTER*256	:: instrument, algid, satid, prodver
	CHARACTER*1024	:: cgfile, outpath_g
        CHARACTER*1280  :: cerroutn
	CHARACTER*100	:: empty
        CHARACTER*50    :: empty_start, empty_stop, granule_number
        CHARACTER*6     :: gran_num_6digit
!
	LOGICAL		:: eogflag, eorbflag, iscan0_set, iscan1_set, &
			   in_window, wrote_output, pps_mode, debug
!
	INTEGER*4	:: nscan, iter, ipos, itime0, itime1, &
			   imin0, imin1, iunit_g, nscanf
!
! QC DEVELOPMENT
        REAL*4          :: sum, rnum_r, avgcrr, rnum, avg
!
	INCLUDE "sat.h"
!
!test
!	LOGICAL		:: in_use
!	INTEGER*4	:: log_lun
	INTEGER*4	:: nofile
	CHARACTER*4	:: cnofile
	nofile = 0
!test
!
        RLATEXT = FLOAT(ILATEXT)
!
!***********************************************************************
!	Load the footprint dimensions.
!
	CALL FOV_DIM  ( NFOV, instrument, w, s, iret )
!
!***********************************************************************
!	Read the granule one scan at a time until all the data are
!	accumulated.
!
	ieofflag = 0
	eogflag = .FALSE.
!
!***********************************************************************
!	Open the GPROF (2AGPROF2021) granule file.
!
	CALL OGPRFILE (cjobid, cgfile, debug, instrument, grnh2021, &
		       iret)
	IF (iret .LT. 0) THEN
	   cerroutn = 'Error opening 2AGPROF file; ' // &
                      'subroutine ogprfile; file ' // cgfile
           IF ( pps_mode ) THEN
              CALL ABTERM_E (cjobid, cerroutn)
           ELSE
 	      istatus = TKmessage (cjobid, TKWARN, cerroutn)
 	      RETURN
           END IF
	END IF
!
!***********************************************************************
!       Read the metadata to get the granule number and the start and
!       stop date/times of the input file (regardless of whether the
!       entire time span is populated with valid  data).
!
!       If the metadata cannot be read, then if in PPS mode, call
!       ABTERM_E and exit, but if not in PPS mode, close the file and
!       return to the calling program to advance to the next file in
!       the list.
!
!       Granule number:
!
        istatus = TKgetMetaString (grnh2021, 'FileHeader', &
                                        'GranuleNumber', granule_number)
        IF ( istatus .NE. TK_SUCCESS ) THEN
           IF ( debug ) &
              WRITE (*,*) 'ERROR: COULD NOT READ METADATA; ', &
                          'TERMINATING EXECUTION; SUBROUTINE GETGPROFG'
           iret = -1
           cerroutn = 'Error: Could not read metadata; ' // &
                      'subroutine getgprofg; file ' // cgfile
           IF ( pps_mode ) THEN
              CALL ABTERM_E (cjobid, cerroutn)
           ELSE
              istatus = TKmessage (cjobid, TKWARN, cerroutn)
              CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
              IF ( iret .LT. 0 ) THEN
                 cerroutn = 'Error closing 2AGPROF file; ' // &
                            'subroutine cgprfile'
                 istatus = TKmessage (cjobid, TKWARN, cerroutn)
              END IF
              RETURN
           END IF
        END IF
!
!       Ensure that the granule number is six characters.
!
	CALL CHAR2INT (cjobid, granule_number, igran_numb)
        WRITE(gran_num_6digit,200) igran_numb
  200   FORMAT(i6.6)
!
!       Granule start date/time:
!
        istatus = TKgetMetaString (grnh2021, 'FileHeader', &
                                'StartGranuleDateTime', empty_start)
!
        IF ( istatus .NE. TK_SUCCESS ) THEN
           IF ( debug ) &
              WRITE (*,*) 'ERROR: COULD NOT READ METADATA; ', &
                          'TERMINATING EXECUTION; SUBROUTINE GETGPROFG'
           iret = -1
           cerroutn = 'Error: Could not read metadata; ' // &
                      'subroutine getgprofg; file ' // cgfile
           IF ( pps_mode ) THEN
              CALL ABTERM_E (cjobid, cerroutn)
           ELSE
              istatus = TKmessage (cjobid, TKWARN, cerroutn)
              CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
              IF ( iret .LT. 0 ) THEN
                 cerroutn = 'Error closing 2AGPROF file; ' // &
                            'subroutine cgprfile'
                 istatus = TKmessage (cjobid, TKWARN, cerroutn)
              END IF
              RETURN
           END IF
        END IF
!
!       Granule stop date/time:
!
        istatus = TKgetMetaString (grnh2021, 'FileHeader', &
                                'StopGranuleDateTime', empty_stop)
!
        IF ( istatus .NE. TK_SUCCESS ) THEN
           IF ( debug ) &
              WRITE (*,*) 'ERROR: COULD NOT READ METADATA; ', &
                          'TERMINATING EXECUTION; SUBROUTINE GETGPROFG'
           iret = -1
           cerroutn = 'Error: Could not read metadata; ' // &
                      'subroutine getgprofg; file ' // cgfile
           IF ( pps_mode ) THEN
              CALL ABTERM_E (cjobid, cerroutn)
           ELSE
              istatus = TKmessage (cjobid, TKWARN, cerroutn)
              CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
              IF ( iret .LT. 0 ) THEN
                 cerroutn = 'Error closing 2AGPROF file; ' // &
                            'subroutine cgprfile'
                 istatus = TKmessage (cjobid, TKWARN, cerroutn)
              END IF
              RETURN
           END IF
        END IF
!
!***********************************************************************
!	Check whether this file is an empty granule, containing
!	metadata only.  If so, then if in PPS mode, call WRITE_EMPTYGRID
!       before closing the file and returning to the calling program.
!       If not in PPS mode, close the file and return to the calling
!       program to advance to the next input file in the list.
!
!	If the metadata cannot be read, then if in PPS mode, call
!       ABTERM_E and exit, but if not in PPS mode, close the file and
!       return to the calling program to advance to the next file in
!       the list.
!
	istatus = TKgetMetaString (grnh2021, 'FileHeader', &
						'EmptyGranule', empty)
	IF ( istatus .NE. TK_SUCCESS ) THEN
	   IF ( debug ) &
	      WRITE (*,*) 'ERROR: COULD NOT READ METADATA; ', &
			  'TERMINATING EXECUTION; SUBROUTINE GETGPROFG'
	   iret = -1
	   cerroutn = 'Error: Could not read metadata; ' // &
		      'subroutine getgprofg; file ' // cgfile
           IF ( pps_mode ) THEN
              CALL ABTERM_E (cjobid, cerroutn)
           ELSE
  	      istatus = TKmessage (cjobid, TKWARN, cerroutn)
  	      CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
  	      IF ( iret .LT. 0 ) THEN
  	         cerroutn = 'Error closing 2AGPROF file; ' // &
  	   		    'subroutine cgprfile'
  	         istatus = TKmessage (cjobid, TKWARN, cerroutn)
  	      END IF
  	      RETURN
           END IF
	END IF
!
	IF ( empty(1:5) .EQ. 'EMPTY' ) THEN
           num_empty = num_empty + 1
  	   cerroutn = 'Encountered empty 2AGPROF file; ' // &
  		      'advancing to next file (if any); ' // &
  		      'subroutine getgprofg; file ' // cgfile
  	   istatus = TKmessage (cjobid, TKWARN, cerroutn)
           IF ( pps_mode ) THEN
!
              CALL WRITE_EMPTYGRID (cjobid, empty_start, empty_stop, &
                           gran_num_6digit, LN, LT, iunit_g, &
                           grnh2021, outpath_g, instrument, &
                           algid, satid, debug, iret)
!
           END IF
  	   CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
  	   IF ( iret .LT. 0 ) THEN
  	      cerroutn = 'Error closing 2AGPROF file; ' // &
                         'subroutine cgprfile'
  	      istatus = TKmessage (cjobid, TKWARN, cerroutn)
  	   END IF
  	   RETURN
	END IF
!
!***********************************************************************
!       Read the product version from the metadata.  This will be
!       passed into ADDMSCAN for use with MHS and ATMS, which have
!       histories of varying widths of "good" data.
!
        istatus = TKgetMetaString (grnh2021, 'FileHeader', &
                                        'ProductVersion', prodver)
!
        IF ( istatus .NE. TK_SUCCESS ) THEN
           IF ( debug ) &
              WRITE (*,*) 'ERROR: COULD NOT READ METADATA; ', &
                          'TERMINATING EXECUTION; SUBROUTINE GETGPROFG'
           iret = -1
           cerroutn = 'Error: Could not read metadata; ' // &
                      'subroutine getgprofg; file ' // cgfile
           IF ( pps_mode ) THEN
              CALL ABTERM_E (cjobid, cerroutn)
           ELSE
              istatus = TKmessage (cjobid, TKWARN, cerroutn)
              CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
              IF ( iret .LT. 0 ) THEN
                 cerroutn = 'Error closing 2AGPROF file; ' // &
                            'subroutine cgprfile'
                 istatus = TKmessage (cjobid, TKWARN, cerroutn)
              END IF
              RETURN
           END IF
        END IF
!
!***********************************************************************
!       Set NOUTER, the number of edge footprints on each side of the
!       swath to be masked, and pass it to ADDMSCAN.  For most
!       instruments, NOUTER = 0.  MHS and ATMS require masking.  These
!       have histories of varying widths of "good" data across the
!       swath, so NOUTER for these instruments is based on the product
!       version obtained from the input file's metadata.
!
!       September 2017 approach for MHS and ATMS:
!       V05A was found to be bad in the outer footprints.  In V05B, CSU
!       changed the quality flag value at these footprints but did not
!       address the underlying issue.  For simplicity as well as
!       continuity in the production archive, we choose to mask (i.e.,
!       set qualityFlag to 3) the same number of footprints in both V05A
!       and V05B.  The A/B data boundary is near the end of the day on
!       28 May 2017, for all four MHS (NOAA-18, NOAA-19, MetOp-A, and
!       MetOp-B) and for NPP/ATMS.  Beginning during September 2017,
!       V05C is an improved CSU method that narrows the number of edge
!       footprints that require masking.  For all other satellites,
!       NOUTER is set to zero.
!
!       Note that production files from arthurhou.pps.eosdis.nasa.gov
!       use the leading "V" in prodver, whereas the near-real-time
!       files from jsimpson.pps.eosdis.nasa.gov do not.  Both flavors
!       are accounted for here.
!
!       A final ELSE condition is included in the "prodver" tests to
!       treat any future versions (or non-conforming value) the same as
!       the most recent version, at least until new guidance can be
!       obtained.  *** NOTE: ITE data encode prodver as ITE### and will
!       therefore fall under this final ELSE.
!
        NOUTER  = 0
        IF  ( TRIM(instrument) .EQ. 'MHS' )  THEN
            IF ( TRIM(prodver) .EQ. 'V05A' .OR. &
                 TRIM(prodver) .EQ. '05A' ) THEN
               NOUTER = 15
            ELSE IF ( TRIM(prodver) .EQ. 'V05B' .OR. &
                      TRIM(prodver) .EQ. '05B' ) THEN
               NOUTER = 15
            ELSE IF ( TRIM(prodver) .EQ. 'V05C' .OR. &
                      TRIM(prodver) .EQ. '05C' ) THEN
               NOUTER = 5
            ELSE
               NOUTER = 5
            END IF
        ELSE IF ( TRIM(instrument) .EQ. 'ATMS' ) THEN
            IF ( TRIM(prodver) .EQ. 'V05A' .OR. &
                 TRIM(prodver) .EQ. '05A' ) THEN
               NOUTER = 18
            ELSE IF ( TRIM(prodver) .EQ. 'V05B' .OR. &
                      TRIM(prodver) .EQ. '05B' ) THEN
               NOUTER = 18
            ELSE IF ( TRIM(prodver) .EQ. 'V05C' .OR. &
                      TRIM(prodver) .EQ. '05C' ) THEN
               NOUTER = 8
            ELSE
               NOUTER = 8
            END IF
        ELSE IF ( TRIM(instrument) .EQ. 'AMSUB' ) THEN
            NOUTER = 5
        END IF
!
!***********************************************************************
!	Read through the entire file, identifying the range of scans to
!	process.  Continue reading the file to fully load the latitude,
!	longitude, and time arrays, even beyond the requested time
!	window, since the nearest-neighbor backward grid scheme will
!	need information from the scan just before and the scan just
!	after each scans-per-orbit block, if such data exist.
!	(Furthermore, if those data do not exist, extrapolation of the
!	navigation will be performed.)  All of this ensures that
!	consecutive orbits will sum to a smooth number-of-counts
!	accumulation, without any "smiles" or "frowns" of 2 amidst the
!	usual field of 1's.
!
!	iscan0 = first scan of requested window
!	iscan1 = last scan of requested window
!
!	NOTE: iscan0, iscan1 begin from index 0, as defined in the
!	2AGPROF file.  Thus, for GMI, for example, the first 2890-scan
!	block in file indices is scans 0-2889.
!
	iscan0 = 0
	iscan1 = 0
	iscan0_set = .FALSE.
	iscan1_set = .FALSE.
	in_window = .FALSE.
	DO j = 1, 8*scans_per_orbit
	DO i = 0, NFOV+1
	   flat(i,j) = FILL
	   flon(i,j) = FILL
	END DO
	END DO
	DO j = 1, 2
	DO i = 0, NFOV+1
	   flatb(i,j) = FILL
	   flonb(i,j) = FILL
	END DO
	END DO
!
!	Initial loop to read through entire file.  Note that j begins
!	at 1.  Therefore, references to iscan0, iscan1 within this loop
!	must subtract 1 since those values are anchored from scan 0.
!	Determination of iscan1 requires subtracting 2, one for the
!	offset and one for backing up one scan before the EOF marker.
!	On the other hand, arrays flat, flon, and jtime are indexed
!	beginning at 1.
!
!	The following loop was originally designed to allow for up to a
!	triple orbit input file.  With the corresponding reduction in
!	value of the nominal scans_per_orbit value implemented in
!	L2AGPROF2014_nngrid.f90 to 3/4 of an orbit - which prevents
!	the data lapping itself at high latitudes - it is necessary here
!	to increase the do loop from 3*scans_per_orbit to 4*scans_per_
!	orbit to preserve a true triple-orbit input file.  To date, this
!	issue has come up only with regard to SSMIS files provided by
!	FNMOC.  To be safe, the loop is being set to 8*scans_per_orbit,
!	which allows for up to 6 true orbits per input file.
!
	DO j = 1, 8*scans_per_orbit
!
	   CALL GETMSCAN(cjobid, grnh2021, scangprof, cgfile, &
			 debug, nsec0, nsec1, in_window, ieofflag, &
			 iret)
!            write(709,*) j, TKendOfFile (grnh2021), TK_EOF, ieofflag
	   IF  ( iret .LT. 0 ) THEN
		cerroutn = 'Error reading 2AGPROF scan; ' // &
			   'subroutine getmscan; file ' // cgfile
                IF ( pps_mode ) THEN
                   CALL ABTERM_E (cjobid, cerroutn)
                ELSE
                   istatus = TKmessage (cjobid, TKWARN, cerroutn)
  		   CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
  		   IF ( iret .LT. 0 ) THEN
  		      cerroutn = 'Error closing 2AGPROF file; ' // &
  			         'subroutine cgprfile'
  		      istatus = TKmessage (cjobid, TKWARN, cerroutn)
  		   END IF
  		   RETURN
                END IF
	   ELSE IF ( ieofflag .EQ. -1 ) THEN
		IF ( iscan0_set ) THEN
		   IF ( .NOT. iscan1_set ) THEN
!                write(709,*) 'JUMPING BACK 2: j was ', j
		      iscan1 = j - 2
!                write(709,*) 'iscan1 is now j-2 = ', iscan1
!
!	10/14/2014: If only one scan was found within the requested
!	window, iscan1 was 2 and then re-set to 0.  However, it must
!	be at least 1 to avoid a crash further in the code.  Adjust
!	the value in this case to 1, which allows the single scan to
!	be gridded to an output file.
!
		      if (iscan1 .le. 0) iscan1 = 1
!
		      iscan1_set = .TRUE.
		   END IF
		ELSE
                   num_outside = num_outside + 1
		   cerroutn = 'Entire file lies outside requested ' // &
			      'time window; subroutine getmscan; ' // &
                              'file ' // cgfile 
  		   istatus = TKmessage (cjobid, TKWARN, cerroutn)
  		   CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, &
  				     iret)
  		   IF ( iret .LT. 0 ) THEN
  		      cerroutn = 'Error closing 2AGPROF file; ' // &
  				 'subroutine cgprfile'
  		      istatus = TKmessage (cjobid, TKWARN, cerroutn)
  		   END IF
  		   RETURN
		END IF
!
		GO TO 30
	   END IF
!
!	Copy over lats and lons, and decrement and increment past end to
!	approximate locations of missing-filled edge guard footprints.
!	Latitudes past the poles are reset to the pole; longitude
!	differences have to account for wrapping (at 180 or 360), then
!	they are wrapped at 180.
!
!       SEP 2017: All lower-edge indices are increased by NOUTER, and
!       all higher-edge indices are decreased by NOUTER.
!
!!	WRITE (*,*) 'getgprofg ready to set lat and lon, j=', j
	   DO i = 1+NOUTER, NFOV-NOUTER
	      flat(i,j) = scangprof.Latitude(i)
	      flon(i,j) = scangprof.Longitude(i)
	   END DO
!
           IF ( scangprof.Latitude(1+NOUTER) .GT. FILLCHK .AND. &
                scangprof.Latitude(2+NOUTER) .GT. FILLCHK ) THEN
	      flat(NOUTER,j) = 2 * scangprof.Latitude(1+NOUTER) - &
                               scangprof.Latitude(2+NOUTER)
           ELSE
              flat(NOUTER,j) = FILL
           END IF
!
	   IF ( flat (NOUTER,j) .GT. 90. ) THEN
	      flat (NOUTER,j) = 90.
	   ELSE IF ( flat (NOUTER,j) .LT. -90. .AND. &
                     flat (NOUTER,j) .GT. FILLCHK ) THEN
	      flat (NOUTER,j) = -90.
	   END IF
!!
	   diff = flon(2+NOUTER,j) - flon (1+NOUTER,j)
	   IF  ( diff .GT. 180. )  THEN
	       diff = diff - 360.
	   ELSE IF  ( diff .LT. -180. )  THEN
	       diff = diff + 360.
	   END IF
	   flon (NOUTER,j) = flon (1+NOUTER,j) - diff
!
	   IF  ( flon(NOUTER,j) .GT. 180. )  THEN
	       flon(NOUTER,j) = flon(NOUTER,j) - 360.
           ELSE IF ( flon(NOUTER,j) .LT. FILLCHK ) THEN
               flon(NOUTER,j) = FILL
	   ELSE IF  ( flon(NOUTER,j) .LT. -180. )  THEN
	       flon(NOUTER,j) = flon(NOUTER,j) + 360.
	   END IF
!!
           IF ( scangprof.Latitude(NFOV-NOUTER) .GT. FILLCHK .AND. &
                scangprof.Latitude(NFOV-1-NOUTER) .GT. FILLCHK ) THEN
	      flat(NFOV+1-NOUTER,j) = &
                               2 * scangprof.Latitude(NFOV-NOUTER) - &
	   		       scangprof.Latitude(NFOV-1-NOUTER)
           ELSE
              flat(NFOV+1-NOUTER,j) = FILL
           END IF
!
	   IF ( flat (NFOV+1-NOUTER,j) .GT. 90. ) THEN
	      flat (NFOV+1-NOUTER,j) = 90.
	   ELSE IF ( flat (NFOV+1-NOUTER,j) .LT. -90. .AND. &
                     flat (NFOV+1-NOUTER,j) .GT. FILLCHK ) THEN
	      flat (NFOV+1-NOUTER,j) = -90.
	   END IF
!!
	   diff = flon(NFOV-NOUTER,j) - flon (NFOV-1-NOUTER,j)
	   IF  ( diff .GT. 180. )  THEN
	       diff = diff - 360.
	   ELSE IF  ( diff .LT. -180. )  THEN
	       diff = diff + 360.
	   END IF
	   flon (NFOV+1-NOUTER,j) = flon (NFOV-NOUTER,j) + diff
!
	   IF  ( flon(NFOV+1-NOUTER,j) .GT. 180. )  THEN
	       flon(NFOV+1-NOUTER,j) = flon(NFOV+1-NOUTER,j) - 360.
           ELSE IF ( flon(NFOV+1-NOUTER,j) .LT. FILLCHK ) THEN
               flon(NFOV+1-NOUTER,j) = FILL
	   ELSE IF  ( flon(NFOV+1-NOUTER,j) .LT. -180. )  THEN
	       flon(NFOV+1-NOUTER,j) = flon(NFOV+1-NOUTER,j) + 360.
	   END IF
!!
	   jtime(j) = ( 60 * scangprof.scanTime.Hour ) + &
		      scangprof.scanTime.Minute + &
		      NINT ( FLOAT ( scangprof.scanTime.Second ) / 60.0)
!
	   IF ( .NOT. iscan0_set ) THEN
	      IF ( in_window ) THEN
!               write(709,*) 'Setting iscan0.  j is ', j
		 iscan0 = j - 1
!
!	10/14/2014: Add protection on iscan0, just in case.
!
		 if (iscan0 .lt. 0) iscan0 = 0
!
!               write(709,*) 'so iscan0 is j-1 = ', iscan0
		 iscan0_set = .TRUE.
	      END IF
	   ELSE IF ( .NOT. iscan1_set ) THEN
	      IF ( .NOT. in_window ) THEN
		 iscan1 = j - 2
!	10/14/2014: Add protection on iscan1, just in case.
!
		 if (iscan1 .le. 0) iscan1 = 1
		 iscan1_set = .TRUE.
	      END IF
	   END IF
!
	END DO
!      write(709,*) 'End 1st read loop: iscan0 = ', iscan0, &
!		    '; iscan1 = ', iscan1
!
   30	CONTINUE
!  30   IF ( iscan0_set .EQ. .FALSE. .OR. iscan1_set .EQ. .FALSE. ) THEN
!	    IF ( debug ) THEN
!		WRITE (*,*) 'ENTIRE FILE PRECEDES REQUESTED ', &
!			    'TIME WINDOW; TERMINATING EXECUTION; ', &
!			    'SUBROUTINE GETGPROFG'
!	    END IF
!	    cerroutn = 'Entire file precedes requested window; ' // &
!		       'advancing to next file (if any); ' // &
!		       'subroutine getgprofg'
!	    istatus = TKmessage (cjobid, TKWARN, cerroutn)
!	    CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
!	    IF ( iret .LT. 0 ) THEN
!	       cerroutn = 'Error closing 2AGPROF file; ' // &
!!			  'subroutine cgprfile'
!	       istatus = TKmessage (cjobid, TKWARN, cerroutn)
!	    END IF
!	    RETURN
!	END IF
!
!***********************************************************************
!	Once done, reposition the file pointer to the desired first
!	scan.  Indexing within the GPROF file begins from zero.
!	Also, reset ieofflag to 0.
!
	istatus = TKseek (grnh2021, iscan0, TK_ABS_SCAN_OFF)
!       write(709,*) 'Repositioned pointer...before re-setting ieofflag'
!       write(709,*) TKendOfFile (grnh2021), TK_EOF, ieofflag
	ieofflag = 0
!
!***********************************************************************
!	Read and accumulate the data until the desired end scan is
!	reached, while breaking up the output into discrete files of
!	a maximum of "scans_per_orbit" scans, to avoid the data lapping
!	itself during the gridding.
!
	nscan  = 0
	iter   = 1
   40	eorbflag = .FALSE.
!
!***********************************************************************
!	Initialize the granule accumulator, output, ambiguous fraction,
!	and footprint "influence" (squared elliptical distance)
!	arrays.  [The filler for influ is the biggest allowed value,
!	since the approach is to be looking for footprints with the
!	smallest value, meaning that they're the closest.]  The
!	main accumulator array is initialized to zero, a holdover from
!	averaging; missings are set in the output step (gprof2021out) if
!	index 2 (number) is zero.  Do the central zone first, then the
!	north and south.
!
	DO  j = 1, LN
	DO  i = 1, LTS
	    gprtimec(i,j) = -1.0
	END DO
	END DO
	DO  k = 1, 4
	DO  j = 1, LN
	DO  i = 1, LTS
	    gprtemp4c(i,j,k) = 0.0
	END DO
	END DO
	END DO
	DO  j = 1, LTS
	DO  i = 1, LN
	    influc (i,j) = 50
	    influ_timec (i,j) = 50
	END DO
	END DO
!
	DO  j = 1, NNSY
	DO  i = 1, NNSX
	    gprtimen(i,j) = -1.0
	    gprtimes(i,j) = -1.0
	END DO
	END DO
	DO  k = 1, 4
	DO  j = 1, NNSY
	DO  i = 1, NNSX
	    gprtemp4n(i,j,k) = 0.0
	    gprtemp4s(i,j,k) = 0.0
	END DO
	END DO
	END DO
	DO  j = 1, NNSY
	DO  i = 1, NNSX
	    influn (i,j) = 50
	    influ_timen (i,j) = 50
	    influs (i,j) = 50
	    influ_times (i,j) = 50
	END DO
	END DO
!
! QC DEVELOPMENT
!
        sum = 0.
        rnum = 0.
        rnum_r = 0. 
        avg = FILL
        avgcrr = FILL
!
!***********************************************************************
!
	CALL GETFST3 (cjobid, grnh2021, scangprof, cgfile, iter, &
		      nsec0, nsec1, iscan0, scans_per_orbit, debug, &
		      ifsmin, eogflag, iret)
	IF (iret .LT. 0) THEN
	   IF ( iret .EQ. -1 ) THEN
	      cerroutn = 'Error resetting 2AGPROF file pointer; ' // &
			 'subroutine getfst3; file ' // cgfile
	   ELSE IF ( iret .EQ. -2 ) THEN
	      cerroutn = 'Error reading 2AGPROF scan; ' // &
			 'subroutine getfst3; file ' // cgfile
	   ELSE IF ( iret .EQ. -3 ) THEN
	      cerroutn = 'Error: bad initial 2AGPROF scan time; ' // &
			 'subroutine getfst3; file ' // cgfile
	   END IF
           IF ( pps_mode ) THEN
              CALL ABTERM_E (cjobid, cerroutn)
           ELSE
  	      istatus = TKmessage (cjobid, TKWARN, cerroutn)
  	      CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
  	      IF ( iret .LT. 0 ) THEN
  	         cerroutn = 'Error closing 2AGPROF file; ' // &
  			    'subroutine cgprfile'
  	         istatus = TKmessage (cjobid, TKWARN, cerroutn)
  	      END IF
  	      RETURN
           END IF
	END IF
!
	DO WHILE ( .NOT. eogflag .AND. .NOT. eorbflag )
	    CALL GETMSCAN ( cjobid, grnh2021, scangprof, cgfile, &
			    debug, nsec0, nsec1, in_window, ieofflag, &
			    iret )
	    IF  ( iret .LT. 0 ) THEN
		cerroutn = 'Error reading 2AGPROF scan; ' // &
			   'subroutine getmscan; file ' // cgfile
                IF ( pps_mode ) THEN
                   CALL ABTERM_E (cjobid, cerroutn)
                ELSE
  		   istatus = TKmessage (cjobid, TKWARN, cerroutn)
  		   CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
  		   IF ( iret .LT. 0 ) THEN
  		      cerroutn = 'Error closing 2AGPROF file; ' // &
  			         'subroutine cgprfile'
  		      istatus = TKmessage (cjobid, TKWARN, cerroutn)
  		   END IF
  		   RETURN
                END IF
	    END IF
	    IF  ( ieofflag .EQ. 0 ) THEN
		nscan = nscan + 1
   		IF ( MOD (nscan, scans_per_orbit) .EQ. 0 ) &
   						   eorbflag = .TRUE.
! MASK OUT AMSR2 SNOW-COVERED SURFACES - 10/18/2018
                IF ( TRIM(instrument) .EQ. 'AMSR2' ) THEN
                   DO i = 1, NFOV
                     IF (scangprof.surfaceTypeIndex(i) .GE. 8 .AND. &
                         scangprof.surfaceTypeIndex(i) .LE. 11 ) THEN
                        scangprof.surfacePrecipitation(i) = FILL
                        scangprof.pixelStatus(i) = -99
                     END IF
                   END DO
                END IF
! QC DEVELOPMENT - 4/26/2018
                DO i = 1, NFOV
                   IF ( scangprof.qualityFlag(i) .LT. 2 .AND. &
                        scangprof.surfacePrecipitation(i) .GE. 0. ) THEN
                      sum =  sum + scangprof.surfacePrecipitation(i)
                      rnum = rnum + 1.
                      IF ( scangprof.surfacePrecipitation(i) .GT. 0.) &
                         rnum_r = rnum_r + 1.
                   END IF
                END DO
   		CALL ADDMSCAN ( cjobid, LN, LTS, ILATEXT, NCEDFRAC, &
   			        NNSFRAC, NNSX, NNSY, NFOV, &
   				NOUTER, scangprof, &
   				debug, ifsmin, w, s, rate_thresh, &
   				influc, influn, influs, influ_timec, &
   			        influ_timen, influ_times, gprtemp4c, &
   			        gprtemp4n, gprtemp4s, gprtimec, &
   			        gprtimen, gprtimes, iret)
!	     write(709,*) 'Back from ADDMSCAN at nscan = ', nscan, &
!			   ', TKendOfFile (grnh2021) = ', &
!			   TKendOfFile (grnh2021)
   		IF (iret .LT. 0 ) THEN
   		   cerroutn = 'Error: Encountered NaN in 2AGPROF; ' // &
   			      'subroutine addmscan; file ' // cgfile
                   IF ( pps_mode ) THEN
                      CALL ABTERM_E (cjobid, cerroutn)
                   ELSE
     		      istatus = TKmessage (cjobid, TKWARN, cerroutn)
     		      CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, &
     				     iret)
     		      IF ( iret .LT. 0 ) THEN
     		         cerroutn = 'Error closing 2AGPROF file; ' // &
     				    'subroutine cgprfile'
     		         istatus = TKmessage (cjobid, TKWARN, cerroutn)
     		      END IF
     		      RETURN
                   END IF
   		END IF
	    ELSE
		eogflag = .TRUE.
	    END IF
	END DO
!	write(709,*) 'End of 2nd read (while) loop, ', &
!		'TKendOfFile (grnh2021) = ', TKendOfFile (grnh2021)
!
!	Grid up the first and last scan using information from the
!	"bogus" scan lines.
!
!	Load flatb, flonb based on navigation from full flat, flon
!	arrays, if available.  If not available, extrapolate.
!
!	The position indicator "ipos" is calculated as multiples of
!	"scans_per_orbit" scans previously read, plus the initial offset
!	value iscan0, plus 1 since ipos is used with flat, flon arrays
!	that begin at scan index 1.
!
!       SEP 2017: Offset edge indices by NOUTER.
!
	ipos = (iter-1)*scans_per_orbit+ iscan0 + 1
	IF ( ipos .GT. 1 ) THEN
	   DO m = 1+NOUTER, NFOV-NOUTER
	      flatb(m, 1) = flat(m, ipos-1)
	      flonb(m, 1) = flon(m, ipos-1)
	   END DO
	ELSE
	   DO m = 1+NOUTER, NFOV-NOUTER
	      flatb(m, 1) = flat(m, ipos) + &
			    (flat(m, ipos) - flat(m, ipos+1))
	      flonb(m, 1) = flon(m, ipos) + &
			    (flon(m, ipos) - flon(m, ipos+1))
	      IF ( flonb(m, 1) .GT. 180. ) THEN
		 flonb(m, 1) = flonb(m, 1) - 360.
              ELSE IF ( flonb(m, 1) .LT. FILLCHK ) THEN
                 flonb(m, 1) = FILL
	      ELSE IF ( flonb(m, 1) .LT. -180. ) THEN
		 flonb(m, 1) = flonb(m, 1) + 360.
	      END IF
	   END DO
	END IF
!
!	Either way, decrement and increment in the along-scan direction
!	past end to approximate locations of missing-filled edge guard
!	footprints.  Latitudes past the poles are reset to the pole;
!	longitude differences have to account for wrapping (at 180 or
!	360), then they are wrapped at 180.
!
!       SEP 2017: Offset edge indices by NOUTER.
!
           IF ( flatb(1+NOUTER,1) .GT. FILLCHK .AND. &
                flatb(2+NOUTER,1) .GT. FILLCHK ) THEN
	      flatb(NOUTER, 1) = 2 * flatb(1+NOUTER, 1) - &
                                 flatb(2+NOUTER, 1)
           ELSE
              flatb(NOUTER, 1) = FILL
           END IF
!
	   IF ( flatb (NOUTER,1) .GT. 90. ) THEN
	      flatb (NOUTER,1) = 90.
	   ELSE IF ( flatb (NOUTER,1) .LT. -90. .AND. &
                     flatb (NOUTER,1) .GT. FILLCHK ) THEN
	      flatb (NOUTER,1) = -90.
	   END IF
!!
	   diff = flonb (2+NOUTER,1) - flonb (1+NOUTER,1)
	   IF  ( diff .GT. 180. )  THEN
	       diff = diff - 360.
	   ELSE IF  ( diff .LT. -180. )  THEN
	       diff = diff + 360.
	   END IF
	   flonb (NOUTER,1) = flonb (1+NOUTER,1) - diff
!
	   IF  ( flonb (NOUTER,1) .GT. 180 )  THEN
	       flonb (NOUTER,1) = flonb (NOUTER,1) - 360.
           ELSE IF ( flonb (NOUTER,1) .LT. FILLCHK ) THEN
               flonb (NOUTER,1) = FILL
	   ELSE IF  ( flonb (0,1) .LT. -180. )  THEN
	       flonb (NOUTER,1) = flonb (NOUTER,1) + 360.
	   END IF
!!
           IF ( flatb (NFOV-NOUTER,1) .GT. FILLCHK .AND. &
                flatb (NFOV-1-NOUTER,1) .GT. FILLCHK ) THEN
	      flatb (NFOV+1-NOUTER,1) = 2 * flatb (NFOV-NOUTER,1) - &
                                        flatb (NFOV-1-NOUTER,1)
           ELSE
              flatb (NFOV+1-NOUTER,1) = FILL
           END IF
!
	   IF ( flatb (NFOV+1-NOUTER,1) .GT. 90. ) THEN
	      flatb (NFOV+1-NOUTER,1) = 90.
	   ELSE IF ( flatb (NFOV+1-NOUTER,1) .LT. -90. .AND. &
                     flatb (NFOV+1-NOUTER,1) .GT. FILLCHK ) THEN
	      flatb (NFOV+1-NOUTER,1) = -90.
	   END IF
!!
	   diff = flonb (NFOV-NOUTER,1) - flonb (NFOV-1-NOUTER,1)
	   IF  ( diff .GT. 180. )  THEN
	       diff = diff - 360.
	   ELSE IF  ( diff .LT. -180. )  THEN
	       diff = diff + 360.
	   END IF
	   flonb (NFOV+1-NOUTER,1) = flonb (NFOV-NOUTER,1) + diff
!
	   IF  ( flonb (NFOV+1-NOUTER,1) .GT. 180 )  THEN
	       flonb (NFOV+1-NOUTER,1) = flonb (NFOV+1-NOUTER,1) - 360.
           ELSE IF ( flonb(NFOV+1-NOUTER,1) .LT. FILLCHK ) THEN
               flonb (NFOV+1-NOUTER,1) = FILL
	   ELSE IF  ( flonb ( NFOV+1-NOUTER,1) .LT. -180. )  THEN
	       flonb (NFOV+1-NOUTER,1) = flonb (NFOV+1-NOUTER,1) + 360.
	   END IF
!
!	Both iscan0 and iscan1 are defined in absolute terms from the
!	start of the file, and the arrays flat, flon, and jtime were
!	loaded in this manner.  However, nscan refers to the offset
!	from iscan0.  Therefore, define nscanf to re-define nscan with
!	respect to the start of the file, not the processing window,
!	for purposes of the bogus scan line tests below.
!
!       SEP 2017: Offset edge indices by NOUTER.
!
	nscanf = nscan + iscan0
!
	IF ( nscanf .LE. iscan1 ) THEN
	   DO m = 1+NOUTER, NFOV-NOUTER
	      flatb(m, 2) = flat(m, nscanf+1)
	      flonb(m, 2) = flon(m, nscanf+1)
	   END DO
	ELSE
	   DO m = 1+NOUTER, NFOV-NOUTER
	      IF ( flat(m, nscanf+1) .NE. FILL ) THEN
		 flatb(m, 2) = flat(m, nscanf+1)
	      ELSE
		 flatb(m, 2) = flat(m, nscanf) + &
			       (flat(m,nscanf) - flat(m, nscanf-1))
	      END IF
	      IF ( flon(m, nscanf+1) .NE. FILL ) THEN
		 flonb(m, 2) = flon(m, nscanf+1)
	      ELSE
		 flonb(m, 2) = flon(m, nscanf) + &
			       (flon(m, nscanf) - flon(m, nscanf-1))
		 IF ( flonb(m, 2) .GT. 180. ) THEN
		    flonb(m, 2) = flonb(m, 2) - 360.
                 ELSE IF ( flonb(m,2) .LT. FILLCHK ) THEN
                    flonb(m, 2) = FILL
		 ELSE IF ( flonb(m, 2) .LT. -180. ) THEN
		    flonb(m, 2) = flonb(m, 2) + 360.
		 END IF
	      END IF
	   END DO
	END IF
!
!	Either way, decrement and increment in the along-scan direction
!	past end to approximate locations of missing-filled edge guard
!	footprints.  Latitudes past the poles are reset to the pole;
!	longitude differences have to account for wrapping (at 180 or
!	360), then they are wrapped at 180.
!
!       SEP 2017: Offset edge indices by NOUTER.
!
           IF ( flatb(1+NOUTER,2) .GT. FILLCHK .AND. &
                flatb(2+NOUTER,2) .GT. FILLCHK ) THEN
	      flatb(NOUTER, 2) = 2 * flatb(1+NOUTER, 2) - &
                                 flatb(2+NOUTER, 2)
           ELSE
              flatb(NOUTER, 2) = FILL
           END IF
!
	   IF ( flatb (NOUTER,2) .GT. 90. ) THEN
	      flatb (NOUTER,2) = 90.
	   ELSE IF ( flatb (NOUTER,2) .LT. -90. .AND. &
                     flatb (NOUTER,2) .GT. FILLCHK ) THEN
	      flatb (NOUTER,2) = -90.
	   END IF
!!
	   diff = flonb (2+NOUTER,2) - flonb (1+NOUTER,2)
	   IF  ( diff .GT. 180. )  THEN
	       diff = diff - 360.
	   ELSE IF  ( diff .LT. -180. )  THEN
	       diff = diff + 360.
	   END IF
	   flonb (NOUTER,2) = flonb (1+NOUTER,2) - diff
!
	   IF  ( flonb(NOUTER,2) .GT. 180 )  THEN
	       flonb(NOUTER,2) = flonb(NOUTER,2) - 360.
           ELSE IF ( flonb(NOUTER,2) .LT. FILLCHK ) THEN
               flonb(NOUTER,2) = FILL
	   ELSE IF  ( flonb(NOUTER,2) .LT. -180. )  THEN
	       flonb(NOUTER,2) = flonb(NOUTER,2) + 360.
	   END IF
!!
           IF ( flatb(NFOV-NOUTER,2) .GT. FILLCHK .AND. &
                flatb(NFOV-1-NOUTER,2) .GT. FILLCHK ) THEN
	      flatb(NFOV+1-NOUTER,2) = 2 * flatb(NFOV-NOUTER,2) - &
                                       flatb(NFOV-1-NOUTER,2)
           ELSE
              flatb(NFOV+1-NOUTER,2) = FILL
           END IF
!
	   IF ( flatb (NFOV+1-NOUTER,2) .GT. 90. ) THEN
	      flatb (NFOV+1-NOUTER,2) = 90.
	   ELSE IF ( flatb (NFOV+1-NOUTER,2) .LT. -90. .AND. &
                     flatb (NFOV+1-NOUTER,2) .GT. FILLCHK ) THEN
	      flatb (NFOV+1-NOUTER,2) = -90.
	   END IF
!!
	   diff = flonb (NFOV-NOUTER,2) - flonb (NFOV-1-NOUTER,2)
	   IF  ( diff .GT. 180. )  THEN
	       diff = diff - 360.
	   ELSE IF  ( diff .LT. -180. )  THEN
	       diff = diff + 360.
	   END IF
	   flonb (NFOV+1-NOUTER,2) = flonb (NFOV-NOUTER,2) + diff
!
	   IF  ( flonb(NFOV+1-NOUTER,2) .GT. 180 )  THEN
	       flonb(NFOV+1-NOUTER,2) = flonb(NFOV+1-NOUTER,2) - 360.
           ELSE IF ( flonb(NFOV+1-NOUTER,2) .LT. FILLCHK ) THEN
               flonb(NFOV+1-NOUTER,2) = FILL
	   ELSE IF  ( flonb(NFOV+1-NOUTER,2) .LT. -180. )  THEN
	       flonb(NFOV+1-NOUTER,2) = flonb(NFOV+1-NOUTER,2) + 360.
	   END IF
!
!	Load the fpb (precip) array for the bogus scan lines to fake
!	values of -98.
!
	DO m = 0, NFOV+1
	   fpb(m, 1) = -98.
	   fpb(m, 2) = -98.
	END DO
!
!	Load the ftb array for the bogus scan lines.  This represents
!	the time in minutes since the start of the current "scans-per-
!	orbit"-scan block.  As such, the first bogus line will always
!	have an imin value of 0.  The formal calculation is therefore
!	commented.  Do the central zone, then the poles.
!       SEP 2017: Change imin to -1 to prevent assigning legitimate
!       zero-minute time (i.e., 0-29 seconds) to the guard scans.
!
!       imin0 = 0
	imin0 = -1
!!
!!	IF ( ipos .GT. 1 ) THEN
!!	   itime0 = jtime(ipos-1)
!!	ELSE
!!	   itime0 = jtime(1)
!!	END IF
!!	IF ( itime0 .GE. ifsmin ) THEN
!!	   imin0 = FLOAT ( itime0 - ifsmin        )
!!	ELSE
!!	   imin0 = FLOAT ( 1440 - ifsmin + itime0 )
!!	END IF
!
	IF ( nscanf+1 .LT. iscan1 ) THEN
	   itime1 = jtime(nscanf+1)
	ELSE
	   itime1 = jtime(iscan1)
	END IF
	IF ( itime1 .GE. ifsmin ) THEN
	   imin1 = FLOAT ( itime1 - ifsmin        )
	ELSE
	   imin1 = FLOAT ( 1440 - ifsmin + itime1 )
	END IF
!
	DO m = 0, NFOV+1
	   ftb(m, 1) = imin0
	   ftb(m, 2) = imin1
	END DO
!
!       Loop through the scans, first computing the FOV orientation
!       along the whole scan, then processing each FOV.  The zones
!       are separated into successive DO's, unlike ADDMSCAN, where the
!	embedded code is sufficiently complicated that it's better to
!	only have one copy of the code and just invoke it footprint-by-
!	footprint where the code is just calls to the FOV2x subroutines.
!
!	The pad of 5 deg. on the test for calling FOV_ORx ensures
!	orientation information for the call to FOV2Gx; that routine
!	checks that each footprint is at least partly on the relevant
!	grid.  An extra scan-center test is done to catch a possible
!	protrusion in the middle due to curved coordinates.
!
!       SEP 2017: Offset edge indices by NOUTER.
!
	DO  n = 1, 2
            IF  ( ( flatb (NOUTER,        n) .LT.  RLATEXT+5. .OR.    &
	            flatb (NFOV/2,        n) .LT.  RLATEXT+5. .OR.    &
		    flatb (NFOV+1-NOUTER, n) .LT.  RLATEXT+5. ) .AND. &
	          ( flatb (NOUTER  ,      n) .GT. -RLATEXT-5. .OR.    &
	            flatb (NFOV/2,        n) .GT. -RLATEXT-5. .OR.    &
		    flatb (NFOV+1-NOUTER, n) .GT. -RLATEXT-5. ) )  THEN
		CALL FOV_ORC ( flonb(0, n), flatb(0, n), ILATEXT, &
			       NCEDFRAC, NFOV, NOUTER, xorc, yorc, iret)
!
		DO  m = NOUTER, NFOV+1-NOUTER
		    CALL FOV2GC_TIMEONLY  ( LN, LTS, ILATEXT, &
			 flonb(m, n), flatb(m, n), ftb(m, n), &
			 xorc (m), yorc (m), w (m), &
			 s (m), influ_timec, gprtimec, iret )
		    ambig = 0.
		    CALL FOV2GC  ( LN, LTS, ILATEXT, flonb(m, n),      &
			 flatb(m, n),  fpb(m, n),       &
			 ambig, xorc (m), yorc (m), w (m), &
			 s (m), influc, gprtemp4c, iret )
		END DO
	    END IF
	END DO
!
	DO  n = 1, 2
            IF  ( flatb (NOUTER,        n) .GT. RLATEXT-5. .OR.  &
	          flatb (NFOV/2,        n) .GT. RLATEXT-5. .OR.  &
	          flatb (NFOV+1-NOUTER, n) .GT. RLATEXT-5. )  THEN
		CALL FOV_ORNS ( flonb(0, n), flatb(0, n), NNSX, NNSY, &
				NNSFRAC, NFOV, NOUTER, xorns, yorns, &
                                iret )
!
		DO  m = NOUTER, NFOV+1-NOUTER
		    CALL FOV2GNS_TIMEONLY  ( ILATEXT, NNSFRAC, NNSX, &
			 NNSY, flonb(m, n), flatb(m, n), ftb(m, n), &
			 xorns (m), yorns (m), w (m), &
			 s (m), influ_timen, gprtimen, iret )
		    ambig = 0.
		    CALL FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, NNSY, &
			 flonb(m, n), flatb(m, n),  fpb(m, n),  &
			 ambig, xorns (m), yorns (m), w (m), &
			 s (m), influn, gprtemp4n, iret )
		END DO
	    END IF
	END DO
!
	DO  n = 1, 2
            IF  ( flatb (NOUTER,        n) .LT. -RLATEXT+5. .OR.  &
	          flatb (NFOV/2,        n) .LT. -RLATEXT+5. .OR.  &
	          flatb (NFOV+1-NOUTER, n) .LT. -RLATEXT+5. )  THEN
		CALL FOV_ORNS ( flonb(0, n), flatb(0, n), NNSX, NNSY, &
				NNSFRAC, NFOV, NOUTER, xorns, yorns, &
                                iret )
!
		DO  m = NOUTER, NFOV+1-NOUTER
		    CALL FOV2GNS_TIMEONLY  ( ILATEXT, NNSFRAC, NNSX, &
			 NNSY, flonb(m, n), flatb(m, n), ftb(m, n), &
			 xorns (m), yorns (m), w (m), &
			 s (m), influ_times, gprtimes, iret )
		    ambig = 0.
		    CALL FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, NNSY, &
			 flonb(m, n), flatb(m, n),  fpb(m, n),  &
			 ambig, xorns (m), yorns (m), w (m), &
			 s (m), influs, gprtemp4s, iret )
		END DO
	    END IF
	END DO
!
!***********************************************************************
!	Shift the central Cylindrical Equi-Distant (CED) band to the
!	global CED grid, then remap the north and south Polar Tangent
!	Planes (PTP) to the global CED grid (using nearest neighbor).
!	Just use the last 4 digits of the "start time in seconds since
!	start of Jan. 2000" to get a unique identifier for the dump-out
!	fields.
!
!test
!!	WRITE (*, *) 'influn 595-605,195-205'
!!	WRITE (*, 555) (( influn (i, j), i=595,605), j=195,205)
!!555	FORMAT ( 11I4 )
!
!!  	nofile = nofile + 1
!!  	WRITE (cnofile, 10) nofile
!! 10	FORMAT ( I4.4 )
!!   	WRITE (*, *) 'cnofile=', cnofile
!Dump influ contents
!!      open(17, file = 'influc'//cnofile, form = 'unformatted', &
!!           access = 'direct', status = 'new', recl = LN*LTS/4)
!!      write(17,rec=1) influc
!!      close(17)
!!      open(17, file = 'influn'//cnofile, form = 'unformatted', &
!!           access = 'direct', status = 'new', recl = NNSX*NNSY/4)
!!      write(17,rec=1) influn
!!      close(17)
!!      open(17, file = 'influs'//cnofile, form = 'unformatted', &
!!           access = 'direct', status = 'new', recl = NNSX*NNSY/4)
!!      write(17,rec=1) influs
!!      open(17, file = 'influ_timec'//cnofile, form = 'unformatted', &
!!           access = 'direct', status = 'new', recl = LN*LTS/4)
!!      write(17,rec=1) influ_timec
!!      close(17)
!!      open(17, file = 'influ_timen'//cnofile, form = 'unformatted', &
!!           access = 'direct', status = 'new', recl = NNSX*NNSY/4)
!!      write(17,rec=1) influ_timen
!!      close(17)
!!      open(17, file = 'influ_times'//cnofile, form = 'unformatted', &
!!           access = 'direct', status = 'new', recl = NNSX*NNSY/4)
!!      write(17,rec=1) influ_times
!!      close(17)
!!	WRITE (*,*) 'done with influns'//cnofile
!	WRITE (*,*) (gprtemp4n(1,j,1),j=1,1200)
!test
!!	OPEN (17, FILE = 'gprtemp4ns'//cnofile, &
!!	     ACCESS = 'STREAM' )
!!	WRITE (17) gprtemp4n,gprtemp4s
!!	CLOSE (17)
!test
!!	WRITE  (*,*) 'About to call ZONES2GLOBE.'
!!	WRITE  (*,*) 'NNSX,NNSY = ', NNSX, NNSY
!!	i = sizeof (influn)
!!	WRITE  (*,*) ' sizeof (influn)=', i
!!	WRITE  (*,17) (influn (i,1), i=1,LN)
!! 17	FORMAT ( 50I2 )
!test
	CALL ZONES2GLOBE (LN, LT, LTS, NNS2CED, NNSFRAC, &
			  NCEDFRAC, NNSX, NNSY, &
			  gprtemp4c, gprtimec, gprtemp4n, gprtimen, &
			  gprtemp4s, gprtimes, gprglobe, iret)
!test
!!	WRITE  (709,*) 'Back from GPROF2021OUT.'
!!	WRITE  (709,*) 'TKendOfFile (grnh2021) = ', TKendOfFile (grnh2021)
!test
!!	OPEN (17, FILE = 'gprglobe'//cnofile, &
!!	     ACCESS = 'STREAM' )
!!	WRITE (17) gprglobe
!!	CLOSE (17)
!!	WRITE (*,*) 'done with gprglobe'//cnofile
!
!***********************************************************************
!	Output the gridded GPROF data.  The prior GMIGAVG isn't needed
!	because averaging isn't done for nearest neighbor, and the
!	rounding that was done in GMIGAVG is repeated in GPROF2021OUT
!	in the process of going to scaled INTEGER*2.
!
!test
!        in_use  = .TRUE.
!        log_lun = 2
!        DO  WHILE ( in_use )
!            log_lun = log_lun + 1
!            INQUIRE ( log_lun, OPENED=in_use )
!        END DO
!        OPEN ( log_lun, file = 'log.out', err = 218 )
!        GO TO 219
!  218   WRITE(*,*) &
!           'ERROR: PROBLEM OPENING LOG FILE log.out', &
!           '; TERMINATING EXECUTION; PROGRAM L2AGPROF2021_nngrid'
!        STOP
!
!  219   CONTINUE
!	write(709,*) 'About to call GPROF2021OUT.'
!	write(709,*) 'TKendOfFile (grnh2021) = ', TKendOfFile (grnh2021)
!	write(709,*) 'nscanf = ', nscanf
!
!***********************************************************************
!       QC: Compute the granule-average conditional rate, in order to
!       suppress writing output in rare cases where the input data are
!       bad.
!
!       Testing on a week of 2003 data from the TRMM era and a week of
!       2017 data from the GPM era yielded maximum conditional rates of
!       ~0.5-0.8 mm/hr for most sensors, except 1.36 for TMI and 1.08
!       for GMI.  For the run script, a suggested value for max_crr_avg
!       is 2.00.
!
!       Since this code is also used in RT, where GMI granules are
!       5 minutes = 160 scans, it is conceivable that a pass directly
!       over a tropical cyclone will exceed max_crr_avg.  Therefore,
!       the test is applied only when nscan > 240 scans.
!
  426   IF ( rnum .GT. 0. ) THEN
           avg = sum/rnum
           IF ( rnum_r .GT. 0.) avgcrr = sum/rnum_r
!          WRITE(426,300) TRIM(cgfile), avg, avgcrr, rnum, rnum_r, nscan
! 300      FORMAT(a,f7.2,f7.2,f9.0,f9.0,i8)
        END IF
!
        IF ( avgcrr .GT. crrmax .AND. nscan .GT. 240 ) THEN
           iret = -9
           GO TO 220
        END IF
!
!***********************************************************************
!
   	CALL GPROF2021OUT (cjobid, LN, LT, iunit_g, gprglobe, &
   			   grnh2021, outpath_g, iter, iscan0, &
   			   instrument, scans_per_orbit, algid, satid, &
   			   nscanf, gran_num_6digit, debug, iret)
!	write(709,*) 'Back from GPROF2021OUT.'
!	write(709,*) 'TKendOfFile (grnh2021) = ', TKendOfFile (grnh2021)
  220   CONTINUE
	IF (iret .LT. 0) THEN
	   IF ( iret .EQ. -1 ) THEN
	      cerroutn = 'Error backing up one scan within input ' // &
			 'GPROF granule; subroutine gprof2021out; ' // &
                         'file ' // TRIM(cgfile)
	   ELSE IF ( iret .EQ. -2 ) THEN
	      cerroutn = 'Error reading GPROF scan; ' // &
			 'subroutine gprof2021out; file ' // &
                         TRIM(cgfile)
	   ELSE IF ( iret .EQ. -3 ) THEN
	      cerroutn = 'Error rewinding input GPROF granule; ' // &
			 'subroutine gprof2021out; file ' // &
                         TRIM(cgfile)
	   ELSE IF ( iret .EQ. -4 ) THEN
	      cerroutn = 'Error opening GPROF gridded file; ' // &
			 'subroutine gprof2021out; file ' // &
                         TRIM(cgfile)
	   ELSE IF ( iret .EQ. -5 ) THEN
	      cerroutn = 'Error writing GPROF gridded file; ' // &
			 'subroutine gprof2021out; file ' // &
                         TRIM(cgfile)
	   ELSE IF ( iret .EQ. -6 ) THEN
	      cerroutn = 'Error closing GPROF gridded file; ' // &
			 'subroutine gprof2021out; file ' // &
                         TRIM(cgfile)
	   ELSE IF ( iret .EQ. -7 ) THEN
	      cerroutn = 'Error writing GPROF granule_list_file; ' // &
			 'subroutine gprof2021out; file ' // &
                         TRIM(cgfile)
	   ELSE IF ( iret .EQ. -8 ) THEN
	      cerroutn = 'Error resetting pointer within input ' // &
			 'GPROF granule; subroutine gprof2021out; ' // &
                         'file ' // TRIM(cgfile)
           ELSE IF ( iret .EQ. -9 ) THEN
              cerroutn = 'Error! Max granule-avg conditional rate ' // &
                         'exceeded; no output produced; file ' // &
                         TRIM(cgfile)
	   END IF
           IF ( pps_mode ) THEN
              CALL ABTERM_E (cjobid, cerroutn)
           ELSE
  	      istatus = TKmessage (cjobid, TKWARN, cerroutn)
  	      CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
              iret = 0
  	      IF ( iret .LT. 0 ) THEN
  	         cerroutn = 'Error closing 2AGPROF file; ' // &
  			    'subroutine cgprfile'
  	         istatus = TKmessage (cjobid, TKWARN, cerroutn)
  	      END IF
  	      RETURN
           END IF
        ELSE
           wrote_output = .TRUE.
	END IF
!
!!	"Peek ahead" to the next scan to make sure it's not the EOF
!!	marker.  If it is, close the file rather than looping back up.
!!	If it is not, then re-set the pointer back one record (to
!!	where it was before this call to GETMSCAN), then loop back up.
!!	Note that the call to GPROF2021OUT re-set the pointer to the
!!	beginning of the block (for obtaining time information), so
!!	the pointer must be re-set here before peeking ahead one scan.
!!
!!	ipos = ( iter )*scans_per_orbit + iscan0
!!	istatus = TKseek (grnh2021, ipos, TK_ABS_SCAN_OFF)
!!        write(708,*) 'Peeking ahead one scan:'
!!	CALL GETMSCAN ( cjobid, grnh2021, scangprof, cgfile, &
!!			debug, nsec0, nsec1, in_window, ieofflag, &
!!			iret )
!!        write(708,*) 'ieofflag = ', ieofflag
!
!!	IF (.NOT. eogflag .AND. ieofflag .EQ. 0) THEN
!       write(709,*) 'TKendOfFile (grnh2021) = ', TKendOfFile (grnh2021)
!       write(709,*) 'TK_EOF = ', TK_EOF
	IF (.NOT. eogflag .AND. TKendOfFile (grnh2021) .NE. TK_EOF) THEN
!! 	   istatus = TKseek (grnh2021, -1, TK_REL_SCAN_OFF)
!!!	IF (.NOT. eogflag) THEN
	   iter = iter + 1
	   GO TO 40
	END IF
!
!***********************************************************************
!	Close the 2AGPROF granule file.
!
	CALL CGPRFILE (cjobid, cgfile, debug, grnh2021, iret)
	IF ( iret .LT. 0 ) THEN
	   cerroutn = 'Error closing 2AGPROF file; ' // &
                      'subroutine cgprfile; file ' // cgfile
           IF ( pps_mode ) THEN
              CALL ABTERM_E (cjobid, cerroutn)
           ELSE
  	      istatus = TKmessage (cjobid, TKWARN, cerroutn)
           END IF
	END IF
!
	RETURN
	END
