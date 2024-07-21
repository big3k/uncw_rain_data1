	SUBROUTINE ADDMSCAN (cjobid, LN, LTS, ILATEXT, NCEDFRAC, &
			     NNSFRAC, NNSX, NNSY, NFOV, &
			     NOUTER, scangprof, &
			     debug, ifsmin, w, s, rate_thresh, &
			     influc, influn, influs, influ_timec, &
			     influ_timen, influ_times, gprtemp4c, &
			     gprtemp4n, gprtemp4s, gprtimec, &
			     gprtimen, gprtimes, iret)
!
!***********************************************************************
!* ADDMSCAN	Accumulate and grid the GPROF observations	       *
!*								       *
!* Called by:	GETGPROFG					       *
!*   								       *
!* Calls:	FOV_ORC, FOV_ORNS, FOV2GC_TIMEONLY, FOV2GNS_TIMEONLY,  *
!*		FOV2GC, FOV2GNS					       *
!*  								       *
!* ADDMSCAN accumulates the valid observation data from a GPROF scan   *
!* into the GPROF granule accumulator array.  ADDMSCAN also counts the *
!* number of valid observations.				       *
!*  								       *
!* Input parameters:						       *
!*  cjobid	CHAR	PPS job ID				       *
!*  LN		INT	number of longitudinal grid boxes	       *
!*  LTS		INT	number of latitudinal grid boxes of data area  *
!*  ILATEXT     INT	starting latitude of data extent in degrees    *
!*		  	(assume symmetrical)			       *
!*  NCEDFRAC	INT	NS grid size as gridboxes per degree	       *
!*  NNSFRAC	INT	NS grid size as gridboxes per degree	       *
!*  NNSX	INT	X size of north and south PTP		       *
!*  NNSY	INT	Y size of north and south PTP		       *
!*  NFOV	INT	number of fields-of-view per scan	       *
!*  NOUTER      INT     number of edge footprints to mask (each side)  *
!*  scangprof	RECORD	structure containing GPROF scan information    *
!*  debug	LOGICAL flag indicating if data fields and messages    *
!*			should be output for debugging purposes	       *
!*  ifsmin	INT	scan time of first scan in minutes since       *
!*			beginning of UTC day			       *
!*  w		REAL	FOV width (cross-scan) in km		       *
!*  s		REAL	FOV length (along-scan) in km		       *
!*  rate_thresh	REAL	precipitation rate threshold (mm/h)	       *
!* 								       *
!* Input and output parameters:					       *
!*  influc	INT1	Footprint "influence" (squared elliptical      *
!*			distance) array, central zone		       *
!*  influn	INT1	Footprint "influence" (squared elliptical      *
!*			distance) array, north zone		       *
!*  influs	INT1	Footprint "influence" (squared elliptical      *
!*			distance) array, south zone		       *
!*  influ_timec	INT1	Footprint "influence" for time field, central  *
!*			zone					       *
!*  influ_timen	INT1	Footprint "influence" for time field, north    *
!*			zone					       *
!*  influ_times	INT1	Footprint "influence" for time field, south    *
!*			zone					       *
!*  gprtemp4c	REAL4*	Work array size (LTS,LN,4) [note that the      *
!*			index order is lat,lon,variables], central     *
!*			zone.  The variables are:		       *
!*			  precip (in mm/hr)			       *
!*			  number of footprints (0 or 1)		       *
!*			  number of raining footprints (0 or 1)	       *
!*			  number of ambiguous footprints (0 or 1)      *
!*  gprtemp4n	REAL4*	Work array size (NNSX,NNSY,4) [note that the   *
!*			index order is PTPx,PTPy,variables], north     *
!*			zone.  Variables are as for gprtemp4c.	       *
!*  gprtemp4s	REAL4*	Work array size (NNSX,NNSY,4) [note that the   *
!*			index order is PTPx,PTPy,variables], south     *
!*			zone.  Variables are as for gprtemp4c.	       *
!*  gprtimec	REAL4*	Work array size (LTS,LN) [note that the        *
!*			index order is lat,lon,variables] of time      *
!*			since start of orbit (in min), central zone.   *
!*  gprtimen	REAL4*	Work array size (NNSX,NNSY) [note that the     *
!*			index order is PTPx,PTPy,variables] of time    *
!*			since start of orbit (in min), north zone.     *
!*  gprtimes	REAL4*	Work array size (NNSX,NNSY) [note that the     *
!*			index order is PTPx,PTPy,variables] of time    *
!*			since start of orbit (in min), south zone.     *
!* 								       *
!* Output parameter:						       *
!*  iret	INT	Return code: 0 = normal			       *
!*				    -1 = stop			       *
!*								       *
!**								       *
!* Log:								       *
!* D.Bolvin/SSAI	03/00	First Version			       *
!* D.Bolvin/SSAI	05/01	Adapt to 3B-42 (V6)		       *
!* D.Bolvin/SSAI	10/03	Redo ambiguous screening	       *
!* D.Bolvin/SSAI	11/03	Check for NaN			       *
!* D.Bolvin/SSAI	08/09	Integrate PPS toolkit API	       *
!* D.Bolvin/SSAI	11/09	Add PPS job ID			       *
!* D.Bolvin/SSAI	12/09	Revamp screening based on V7 2A-12     *
!* D.Bolvin/SSAI	06/11	Variable data resolution	       *
!* D.Bolvin/SSAI	06/11	Adapt API to GPM TK		       *
!* G.Huffman/612	06/13	Adapt addmscan; gmitemp now gets loaded*
!*				in fov2g; influ has to be held in      *
!*				getgmig				       *
!* D.Bolvin/SSAI	08/13	Adapt from TMI			       *
!* E.Nelkin/SSAI	08/13	Eliminate TK_3IMERGmessages.h	       *
!* E.Nelkin/SSAI	09/13	Replace ABTERM_E with iret/RETURN      *
!* E.Nelkin/SSAI	11/13	Set desert (pixelStatus=12) precip to  *
!*				unambiguous zero		       *
!* E.Nelkin/SSAI	11/13	Invoke an initial fov2g_timeonly at    *
!*				every FOV to ensure complete time field*
!* E.Nelkin/SSAI	11/13	Update values of qualityFlag (was 1-3, *
!*				now 0-2)			       *
!* E.Nelkin/SSAI	12/13	Generic version - Use instruments.h    *
!* E.Nelkin/SSAI	01/14	Include generic "sat.h"		       *
!* E.Nelkin/SSAI	01/14	Pass in NFOV instead of setting it in  *
!*				PARAMETER statement; pass it to FOV_OR *
!* E.Nelkin/SSAI	04/14	TEMPORARILY change ocean qualityFlag   *
!*				test to .LE. 2 instead of .LT. 2, for  *
!*				initial post-launch CSU test data      *
!* E.Nelkin/SSAI	07/14	TEMPORARILY disable ambiguous flagging *
!*				over land, for initial post-launch CSU *
!*				test data			       *
!* E.Nelkin/SSAI	10/14	TEMPORARILY accept all surface types   *
!*				since estimates are provided	       *
!*				everywhere; 1 is considered ocean and  *
!*				everything else (2-15) is considered   *
!*				land; threshold ocean and land at      *
!*				0.1 mm/hour.			       *
!* E.Nelkin/SSAI	02/16	Accept quality flags 0 and 1, reject 2 *
!*				over ocean and land (per D. Randel);   *
!*				results in no land ambiguous flagging  *
!* G.Huffman/612	05/16	Introduce N&S zones from TRMM gridder  *
!* E.Nelkin/SSAI	08/16	Use sensor-specific rate_thresh, set in*
!*				main program, passed in via GETGPROFG  *
!* E.Nelkin/SSAI        02/17   For V04, accept all quality flags      *
!*                              (per D. Randel)                        *
!* E.Nelkin/SSAI        02/17   Preserve/grid missing precip; addresses*
!*                              data drop-out "smiles" and "frowns"    *
!* E.Nelkin/SSAI        03/17   Removed pixelStatus = 12 desert test   *
!* E.Nelkin/SSAI        03/17   For V05, accept quality flags 0 and 1  *
!* G.Huffman/612	05/17	Edge-guard footprints		       *
!* G.Huffman/612	06/17	Drop juicy MHS outer-swath footprints  *
!* G.Huffman/612	06/17	Set edge-guard precip to -9998., drop  *
!*				all outer-swath (qualityFlag=3)	       *
!* E.Nelkin/SSAI        09/17   Pass in NOUTER; use with MHS and ATMS  *
!*                              to mask known-bad edge footprints      *
!* E.Nelkin/SSAI        09/17   Set qualityFlag and ipix on NOUTER     *
!*                              footprints to zero to allow processing *
!*                              of edges                               *
!* E.Nelkin/SSAI        09/17   Add bounds checks on lat/lon at edges  *
!* E.Nelkin/SSAI        09/17   Fix bug: change +5 to -5 on 5th of 6   *
!*                              checks in IF test for CALL FOV_ORC     *
!* E.Nelkin/SSAI        09/17   Set iqual to 0 at bogus edge footprints*
!* E.Nelkin/SSAI        09/17   Define/use RLATEXT as FLOAT(ILATEXT)   *
!* E.Nelkin/SSAI        09/17   Offset edges by NOUTER (MHS/ATMS)      *
!* E.Nelkin/SSAI        09/17   Move an END IF that erroneously closed *
!*                              off the fp test too soon               *
!* E.Nelkin/SSAI        05/21   GPROF2020 version: use precip yes/no   *
!*                              flag (assigned to iyn)                 *
!* E.Nelkin/SSAI        06/21   Land surface types now 2-18            *
!* E.Nelkin/SSAI        06/21   Use logical debug variable             *
!***********************************************************************
!
	IMPLICIT	NONE
!
#include "instruments.h"
!
	INTEGER*4	:: LN, LTS, ILATEXT, NCEDFRAC, &
			   NNSFRAC, NNSX, NNSY, NFOV
        REAL*4          :: RLATEXT
	REAL*4		:: FILL, FILLEDGE, FILLCHK, TLOW, THIGH
	PARAMETER	( FILL     = -9999., &
			  FILLEDGE = -9998., &
			  FILLCHK  = -8888., &
			  TLOW     = 0.,     &
			  THIGH    = 3000. )
!
	INTEGER*4	:: i, istatus, ifsmin, itime, iret, j, NOUTER
!
	REAL*4		:: rain, time, ambig, data
	REAL*4		:: gprtemp4c (LTS, LN, 4), gprtimec (LTS, LN), &
			   gprtemp4n (NNSX, NNSY, 4), &
			   gprtemp4s (NNSX, NNSY, 4), &
			   gprtimen (NNSX, NNSY), gprtimes (NNSX, NNSY)
	REAL*4		:: flon (0:NFOV+1), flat (0:NFOV+1), fp (0:NFOV+1), &
			   xorc (0:NFOV+1), yorc (0:NFOV+1), &
			   xorns (0:NFOV+1), yorns (0:NFOV+1), &
			   w (0:NFOV+1), s (0:NFOV+1)
	INTEGER*4	:: ipix (0:NFOV+1), isfc (0:NFOV+1), &
			   iqual (0:NFOV+1), ipop (0:NFOV+1)
        INTEGER*2       :: iyn(0:NFOV+1)
	INTEGER*1	:: influc (LN, LTS), influ_timec (LN, LTS), &
			   influn (NNSX, NNSY), influs (NNSX, NNSY), &
			   influ_timen (NNSX, NNSY), &
			   influ_times (NNSX, NNSY)
!
	CHARACTER*256	:: cerroutn, cjobid
	LOGICAL*4	:: zonec, zonen, zones, debug
	REAL*4		:: rate_thresh, diff
!
	INCLUDE "sat.h"
!
        RLATEXT = FLOAT(ILATEXT)
!
!***********************************************************************
!	First check if the scan level data quality is acceptable.
!	Next, perform some limit checks on the surface precipitation
!	rate data and accumulate it if it is verified as valid.
!
!	Per Dave Randel (February 2016), the 2AGPROF flags are now
!	treated as follows:
!
!	scangprof.qualityFlag(i) = 0 (good for ocean and land/coast)
!	scangprof.qualityFlag(i) = 1 (acceptable for ocean and land/
!				     coast; caused by non-fatal L1C
!				     error [e.g., warm load calibration]
!				     or by sunglint contamination)
!	scangprof.qualityFlag(i) = 2 (unacceptable for ocean and land/
!				     coast; caused by missing surface
!				     rain or VERY bad L1C quality flag)
!
!       V04 UPDATE (February 2017), per Dave Randel:
!       scangprof.qualityFlag(i) = 2 WITH scangprof.L1CqualityFlag
!         .LT. -120 (acceptable; SSMIS channel drop-outs where retrieval
!         was still made; CSU likely should have set qualityFlag to 1)
!         HOWEVER, the object "L1CqualityFlag" does not exist in the
!         Level 2 HDF5 file.  Therefore, while a version of this code
!         that checks for L1CqualityFlag compiles, when using an
!         appropriate Toolkit version, the code does not run.
!         Dave Randel subsequently advised that for V04, we accept
!         all qualityFlag = 2 cases, which effectively means accepting
!         everything.  Testing of sample October 1, 2014 orbits from
!         all sensors appears to let in acceptable precipitation,
!         generally zeroes over polar latitudes, and reasonable rates
!         for F16 and F18 at other latitudes when qualityFlag = 2.
!
!       V05 (March 2017), per Dave Randel:
!       0 : Pixel is "good" and has the highest confidence of the best
!          retrieval.
!       1 : "Use with caution." Pixels can be set to 1 for the
!          following reasons:
!          - Sunglint is present, RFI, geolocate, warm load
!            or other L1C ’positive value’ quality warning flags.
!          - All sea-ice covered surfaces.
!          - All snow covered surfaces.
!          - Sensor channels are missing, but not critical ones.
!       2 : "Use pixel with extreme care over snow covered surface."
!          This is a special value for snow covered surfaces only.
!          The pixel is set to 2 if the probability of precipitation
!          is of poor quality or indeterminate.  Use these pixels
!          for climatological averaging of precipitation, but not
!          for individual storm scale daily cases.
!       3 : "Use with extreme caution."  Pixels are set to 3 if
!          they have channels missing critical to the retrieval,
!          but the choice has been made to continue the retrieval
!          for the pixel.
!     -99 : Missing value
!
!	scangprof.pixelStatus(i) = 0 (good)
!
!	scangprof.surfacePrecipitation(i) .GE. 0 is redundant with
!	scangprof.pixelStatus(i) = 0, but I'm going to keep it anyway
!
!	scangprof.probabilityOfPrecip(i) is currently not inspected
!	since the pixels are being spatially averaged.
!
!	scangprof.surfaceTypeIndex(i) - 1   	= ocean
!				   	2-15	= "considered" land
!
!	The GPROF data is provided in millimeters/hour.  The low/high
!	limits of 0 mm/hr and 3000 mm/hr are arbitrary.
!
!	NOTE:  The geolocation data within the scan should be within
!	acceptable limits if the geolocation quality flag is zero.
!	However, checks of the geolocation quality are performed before
!	accumulating the associated GPROF data.  This is done to ensure
!	the integrity of the accumulation and clipping scheme.  Any
!	unexpected "bad" geolocation values are counted and the
!	associated GPROF surface precipitation rate value ignored.  The
!	total number of "bad" geolocation values is output to the log
!	file for each scan after each scan has been processed.
!
!       GPROF2020 (May 2021):
!       scangprof.precipitationYesNoFlag is set to 0 for no, 1 for yes.
!       Chris Kummerow and Paula Brown advise using it in conjunction
!       with scangprof.surfacePrecipitation .GT. 0 for defining a pixel
!       as precipitating.
!
!***********************************************************************
!       SEP 2017: This block was an earlier attempt at handling the
!       edges, but it is no longer necessary.
!
!       For MHS and ATMS, assign a qualityFlag value of 0 to the NOUTER
!       edge footprints.  Formerly these were set to 3, but 0 is needed
!       for the subsequent edge computations to trigger.  Assign a fill
!       value for precipitation.  The value of NOUTER has been set in
!       GETGPROFG, based on the instrument and product version.
!
!	IF  ( NOUTER .NE. 0 )  THEN
!	    DO  i = 1, NOUTER
!		scangprof.qualityFlag (i) = 0
!               scangprof.surfacePrecipitation (i) = FILLEDGE
!	    END DO
!	    DO  i = NFOV+1-NOUTER, NFOV
!		scangprof.qualityFlag (i) = 0
!               scangprof.surfacePrecipitation (i) = FILLEDGE
!	    END DO
!	END IF
!
!
!***********************************************************************
!	Set the orientation of the footprints along the scan and set the
!	time since the beginning of the orbit (in minutes). [Pulled out
!	of the detailed IFs as it used to be; might have to go back if
!	you encounter bad times when other data are bad.]
!
!	The pad of 5 deg. on the test for calling FOV_ORx ensures
!	orientation information for the call to FOV2Gx; that routine
!	checks that each footprint is at least partly on the relevant
!	grid.  An extra scan-center test is done to catch a possible
!	protrusion in the middle due to curved coordinates.
!
!	The logicals zonec, zonen, zones allow selective calling of the
!	FOV2Gx routines footprint-by-footprint without repeating the
!	complete IF tests.  The footprint can be in the central and one
!	of the polar zones, but not in both polar zones.
!
!	Start by setting the lat/lon to include the side-guard foot-
!	prints; decrement and increment past end to approximate
!	locations of missing-filled edge guard footprints.  Latitudes
!	past the poles are reset to the pole; longitude differences have
!	to account for wrapping (at 180 or 360), then they are wrapped
!       at 180.
!
!       Go ahead and do the whole range even if NOUTER .GT. 0 to ensure
!       values everywhere.
!
	DO  i = 1, NFOV
	    flon  (i) = scangprof.Longitude (i)
	    flat  (i) = scangprof.Latitude (i)
	    fp    (i) = scangprof.surfacePrecipitation (i)
	    ipix  (i) = scangprof.pixelStatus (i)
	    isfc  (i) = scangprof.surfaceTypeIndex (i)
	    iqual (i) = scangprof.qualityFlag (i)
	    ipop  (i) = scangprof.probabilityOfPrecip (i)
            iyn   (i) = scangprof.precipitationYesNoFlag (i)
	END DO
!
	fp    (0)      = FILLEDGE
	fp    (NFOV+1) = FILLEDGE
	ipix  (0)      = 0
	ipix  (NFOV+1) = 0
	isfc  (0)      = 0
	isfc  (NFOV+1) = 0
	iqual (0)      = 0
	iqual (NFOV+1) = 0
	ipop  (0)      = 0
	ipop  (NFOV+1) = 0
        iyn   (0)      = -99
        iyn   (NFOV+1) = -99
!
!       When NOUTER .GT. 0 (currently, MHS and ATMS), repeat the
!       above assignments at positions NOUTER and NFOV+1-NOUTER,
!       overruling whatever initially appeared in the Level 2 file.
!       The positions outside of these locations out to the true
!       swath edges can retain their initial values, since CSU has
!       set these positions to missing, including their lat/lon values.
!
        IF ( NOUTER .GT. 0 ) THEN
           fp    (NOUTER)        = FILLEDGE
           fp    (NFOV+1-NOUTER) = FILLEDGE
           ipix  (NOUTER)      =   0
           ipix  (NFOV+1-NOUTER) = 0
           isfc  (NOUTER)        = 0
           isfc  (NFOV+1-NOUTER) = 0
           iqual (NOUTER)        = 0
           iqual (NFOV+1-NOUTER) = 0
           ipop  (NOUTER)        = 0
           ipop  (NFOV+1-NOUTER) = 0
           iyn   (NOUTER)        = -99
           iyn   (NFOV+1-NOUTER) = -99
        END IF
!
!       Wrap calculations inside bounds-check test, setting to FILL
!       if out-of-bounds, for footprints NOUTER and NFOV+1-NOUTER.
!       Note that even if one or more flat values are missing, these
!       tests will keep the assigned zones correct.
!
        IF ( flat(1+NOUTER) .GE.  -90. .AND. &
             flat(1+NOUTER) .LE.   90. .AND. &
             flon(1+NOUTER) .GE. -180. .AND. &
             flon(1+NOUTER) .LE.  180. .AND. &
             flat(2+NOUTER) .GE.  -90. .AND. &
             flat(2+NOUTER) .LE.   90. .AND. &
             flon(2+NOUTER) .GE. -180. .AND. &
             flon(2+NOUTER) .LE.  180. ) THEN
	   flat(NOUTER) = 2 * flat(1+NOUTER) - flat(2+NOUTER)
	   IF ( flat (NOUTER) .GT. 90. ) THEN
	      flat (NOUTER) = 90.
	   ELSE IF ( flat (NOUTER) .LT. -90. ) THEN
	      flat (NOUTER) = -90.
	   END IF
!!
	   diff = flon(2+NOUTER) - flon (1+NOUTER)
	   IF  ( diff .GT. 180. )  THEN
	       diff = diff - 360.
	   ELSE IF  ( diff .LT. -180. )  THEN
	       diff = diff + 360.
	   END IF
	   flon (NOUTER) = flon (1+NOUTER) - diff
!
	   IF  ( flon(NOUTER) .GT. 180. )  THEN
	       flon(NOUTER) = flon(NOUTER) - 360.
	   ELSE IF  ( flon(NOUTER) .LT. -180. )  THEN
	       flon(NOUTER) = flon(NOUTER) + 360.
	   END IF
        ELSE
           flat(NOUTER) = FILL
           flon(NOUTER) = FILL
        END IF
!!
        IF ( flat(NFOV-1-NOUTER) .GE.  -90. .AND. &
             flat(NFOV-1-NOUTER) .LE.   90. .AND. &
             flon(NFOV-1-NOUTER) .GE. -180. .AND. &
             flon(NFOV-1-NOUTER) .LE.  180. .AND. &
             flat(NFOV-NOUTER  ) .GE.  -90. .AND. &
             flat(NFOV-NOUTER  ) .LE.   90. .AND. &
             flon(NFOV-NOUTER  ) .GE. -180. .AND. &
             flon(NFOV-NOUTER  ) .LE.  180. ) THEN
	   flat(NFOV+1-NOUTER) = 2 * flat(NFOV-NOUTER) - &
                                 flat(NFOV-1-NOUTER)
	   IF ( flat (NFOV+1-NOUTER) .GT. 90. ) THEN
	      flat (NFOV+1-NOUTER) = 90.
	   ELSE IF ( flat (NFOV+1-NOUTER) .LT. -90. ) THEN
	      flat (NFOV+1-NOUTER) = -90.
	   END IF
!!
	   diff = flon(NFOV-NOUTER) - flon (NFOV-1-NOUTER)
	   IF  ( diff .GT. 180. )  THEN
	       diff = diff - 360.
	   ELSE IF  ( diff .LT. -180. )  THEN
	       diff = diff + 360.
	   END IF
	   flon (NFOV+1-NOUTER) = flon (NFOV-NOUTER) + diff
!
	   IF  ( flon(NFOV+1-NOUTER) .GT. 180. )  THEN
	       flon(NFOV+1-NOUTER) = flon(NFOV+1-NOUTER) - 360.
	   ELSE IF  ( flon(NFOV+1-NOUTER) .LT. -180. )  THEN
	       flon(NFOV+1-NOUTER) = flon(NFOV+1-NOUTER) + 360.
	   END IF
        ELSE
           flat(NFOV+1-NOUTER) = FILL
           flon(NFOV+1-NOUTER) = FILL
        END IF
!
	IF  ( ( flat (NOUTER       ) .LT.  RLATEXT+5. .OR.    &
		flat (NFOV/2       ) .LT.  RLATEXT+5. .OR.    &
		flat (NFOV+1-NOUTER) .LT.  RLATEXT+5. ) .AND. &
	      ( flat (NOUTER       ) .GT. -RLATEXT-5. .OR.    &
		flat (NFOV/2       ) .GT. -RLATEXT-5. .OR.    &
	        flat (NFOV+1-NOUTER) .GT. -RLATEXT-5. ) )  THEN
	    CALL FOV_ORC  ( flon, flat, ILATEXT, NCEDFRAC, NFOV, &
			    NOUTER, xorc, yorc, iret )
	    zonec = .TRUE.
	  ELSE
	    zonec = .FALSE.
	END IF
!
        IF  ( flat (NOUTER       ) .GT. RLATEXT-5. .OR.  &
	      flat (NFOV/2       ) .GT. RLATEXT-5. .OR.  &
	      flat (NFOV+1-NOUTER) .GT. RLATEXT-5. )  THEN
	    CALL FOV_ORNS  ( flon, flat, NNSX, NNSY, NNSFRAC, NFOV, &
			     NOUTER, xorns, yorns, iret )
	    zonen = .TRUE.
	    zones = .FALSE.
	  ELSE IF  ( flat (NOUTER       ) .LT. -RLATEXT+5. .OR.  &
	             flat (NFOV/2       ) .LT. -RLATEXT+5. .OR.  &
	             flat (NFOV+1-NOUTER) .LT. -RLATEXT+5. )  THEN
	    CALL FOV_ORNS  ( flon, flat, NNSX, NNSY, NNSFRAC, NFOV, &
			     NOUTER, xorns, yorns, iret )
	    zonen = .FALSE.
	    zones = .TRUE.
	  ELSE
	    zonen = .FALSE.
	    zones = .FALSE.
	END IF
!
	itime = ( 60 * scangprof.scanTime.Hour) + &
		  scangprof.scanTime.Minute + &
		  NINT ( FLOAT ( scangprof.scanTime.Second ) / 60.0 )
!
	IF  ( itime .GE. ifsmin )  THEN
	      time = FLOAT ( itime - ifsmin        )
	  ELSE
	      time = FLOAT ( 1440 - ifsmin + itime )
	END IF
! test
!!	WRITE (*,*) 'lon1,lat1,zonec,n,s ', scangprof.Longitude (1), &
!!	            scangprof.Latitude (1),zonec,zonen,zones
!
!***********************************************************************
!	Loop through all footprints aside from the NOUTER ones on each
!       edge and accumulate the data.
!	Start with some quality checks on precip and location.  In each
!	surface type, set rain and ambiguous, and then call FOV2G to put
!	the values in gprtemp4 in one or more grid boxes.  This potential
!	for touching several grid boxes in the backward nearest neighbor
!	approach, and the many tests that might exclude a footprint are
!	the reasons why FOV2G must be coded in each surface type's
!	section.
!
	DO  i = NOUTER, NFOV+1-NOUTER
!
!	Begin by invoking the time-only version of FOV2Gx at *every* FOV
!	in order to ensure a complete time field, regardless of
!	pixelStatus.
!
!       SEP 2017: Wrap these tests under a lat/lon bounds check.
!
            IF  ( flat(i) .GE.  -90. .AND. flat(i) .LE.  90. .AND. &
                  flon(i) .GE. -180. .AND. flon(i) .LE. 180. ) THEN
	        IF  ( zonec )  CALL FOV2GC_TIMEONLY  ( LN, LTS, &
		               ILATEXT, flon (i), flat (i),  &
		               time, xorc (i), yorc (i),  &
		               w (i), s (i), influ_timec, &
		               gprtimec, iret )
	        IF  ( zonen )  CALL FOV2GNS_TIMEONLY  ( ILATEXT, &
		               NNSFRAC, NNSX, NNSY, &
		               flon (i), flat (i),  &
		               time, xorns (i), yorns (i),  &
		               w (i), s (i), influ_timen, gprtimen, &
                               iret )
	        IF  ( zones )  CALL FOV2GNS_TIMEONLY  ( ILATEXT, &
		               NNSFRAC, NNSX, NNSY, &
		               flon (i), flat (i),  &
		               time, xorns (i), yorns (i),  &
		               w (i), s (i), influ_times, gprtimes, &
                               iret )
           END IF
	END DO
!
	DO  i = NOUTER, NFOV+1-NOUTER
!
	    IF  (ipix (i) .EQ. 0 ) THEN
!
		IF  (fp (i) .GT. FILLCHK) THEN
!
!***********************************************************************
!	            Special code added to check for NaN without bombing
!	            out.  IF NaN is found, go to next pixel.
!
		    data = fp(i)
		    IF  (ISNAN (data)) THEN
			IF  (debug) THEN
			    WRITE (*,*) 'Encountered NaN'
			END IF
!			cerroutn = 'Error: Encountered NaN in ' // &
!				   '2AGPROF; subroutine addmscan'
!			CALL ABTERM_E (cjobid, cerroutn)
			iret = -1
			RETURN
		    END IF
!
!		    IF  ((flat(i) .GE. -(FLOAT(ILATEXT))) .AND. &
!			 (flat(i) .LE. FLOAT(ILATEXT)) .AND. &
!			 (flon(i) .GE. -180.0) .AND. &
!			 (flon(i) .LE. 180.0)) THEN
		    IF  ((flat(i) .GE.   -90.) .AND. &
			 (flat(i) .LE.    90.) .AND. &
			 (flon(i) .GE. -180.) .AND. &
			 (flon(i) .LE.  180.)) THEN
!
			IF  ((fp(i) .GE. TLOW) .AND. &
			     (fp(i) .LE. THIGH)) THEN
!
!	Ocean.  Ambiguous is zero.
!
	IF  (isfc (i) .EQ. 1) THEN
!
!       V04, February 2017 update - change .LT. to .LE.
!       V05, March 2017 - use .LT. 2
 	    IF  (iqual (i) .LT. 2) THEN
!
!	    Auditing on POP commented out and replaced by thresholding
!	    on rate until CSU gets POP straightened out.
!
!		IF  (ipop (i) .GE. 50) THEN
!		    rain = fp (i)
!		  ELSE
!		    rain = 0.0
!		END IF
!
!		IF  (fp (i) .GE. 0.1) THEN
!		IF  (fp (i) .GE. rate_thresh) THEN
!!
!! LIKELY WHAT WE WANT:
                IF  (iyn (i) .EQ. 1 .AND. fp (i) .GE. rate_thresh) THEN
!!
!! TEST 1 TO ASSESS IMPACT OF YesNo FLAG:
!!              IF  (iyn (i) .EQ. 1) THEN
		    rain = fp (i)
		  ELSE
		    rain = 0.0
		END IF
!
!YDT 7/21/24 hack: replace ambig value with surfacetype
		! ambig = 0.
            ambig = isfc(i) 

		IF  ( zonec )  CALL FOV2GC  ( LN, LTS, ILATEXT, &
			            flon (i), flat (i),   &
				    rain,  ambig, xorc (i), &
				    yorc (i), w (i), s (i), influc, &
				    gprtemp4c, iret )
		IF  ( zonen )  CALL FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, &
			            NNSY, flon (i), flat (i),   &
				    rain,  ambig, xorns (i), &
				    yorns (i), w (i), s (i), influn, &
				    gprtemp4n, iret )
		IF  ( zones )  CALL FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, &
			            NNSY, flon (i), flat (i),   &
				    rain,  ambig, xorns (i), &
				    yorns (i), w (i), s (i), influs, &
				    gprtemp4s, iret )
!
	    END IF
!
!	  Land.
!
	  ELSE IF  ((isfc (i) .GE. 2) .AND. &
		    (isfc (i) .LE. 18)) THEN
!
!       V04, February 2017 update - change .LT. to .LE.
!       V05, March 2017 - use .LT. 2
!           IF  (scangprof.qualityFlag(i) .LT. 2) THEN
	    IF  (iqual(i) .LT. 2) THEN
!
!		IF  (fp (i) .GE. 0.1) THEN
!		IF  (fp (i) .GE. rate_thresh) THEN
!!
!! LIKELY WHAT WE WANT:
                IF  (iyn (i) .EQ. 1 .AND. fp (i) .GE. rate_thresh) THEN
!!
!! TEST 1 TO ASSESS IMPACT OF YesNo FLAG:
!!                IF (iyn (i) .EQ. 1) THEN
		    rain = fp (i)
		  ELSE
		    rain = 0.0
		END IF
!
!YDT 7/21/24 hack: replace ambig value with surfacetype
		! ambig = 0.
            ambig = isfc(i) 

		IF  ( zonec )  CALL FOV2GC  ( LN, LTS, ILATEXT, &
			            flon (i), flat (i),   &
				    rain,  ambig, xorc (i), &
				    yorc (i), w (i), s (i), influc, &
				    gprtemp4c, iret )
		IF  ( zonen )  CALL FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, &
			            NNSY, flon (i), flat (i),   &
				    rain,  ambig, xorns (i), &
				    yorns (i), w (i), s (i), influn, &
				    gprtemp4n, iret )
		IF  ( zones )  CALL FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, &
			            NNSY, flon (i), flat (i),   &
				    rain,  ambig, xorns (i), &
				    yorns (i), w (i), s (i), influs, &
				    gprtemp4s, iret )
!
!	      ELSE IF  (iqual (i) .EQ. 2) THEN
!
!		rain = fp (i)
!
!! ULTIMATELY WANT:
!		ambig = 1.
!
!		IF  ( zonec )  CALL FOV2GC  ( LN, LTS, ILATEXT, &
!			            flon (i), flat (i),   &
!				    rain,  ambig, xorc (i), &
!				    yorc (i), w (i), s (i), influc, &
!				    gprtemp4c, iret )
!		IF  ( zonen )  CALL FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, &
!			            NNSY, flon (i), flat (i),   &
!				    rain,  ambig, xorns (i), &
!				    yorns (i), w (i), s (i), influn, &
!				    gprtemp4n, iret )
!		IF  ( zones )  CALL FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, &
!			            NNSY, flon (i), flat (i),   &
!				    rain,  ambig, xorns (i), &
!				    yorns (i), w (i), s (i), influs, &
!				    gprtemp4s, iret )
!
!
	    END IF
!
	END IF
!
			END IF
!
		    END IF
!
! SEP 2017: Move this END IF, which erroneously prematurely closed off
! the IF (fp (i) .GT. FILLCHK) test.
!		END IF
!
!           Preserve/grid other cases of missing precipitation; this
!           is aimed at data drop-outs ("smiles" and "frowns").
!	    6/26/17 shift to passing through the fp (i) value, instead
!	    of resetting it to FILL.
!           9/13/17: change to ELSE
!
!           ELSE IF (fp (i) .LT. FILLCHK) THEN
	    ELSE
	      rain = fp (i)
!YDT 7/21/24 hack: replace ambig value with surfacetype
		! ambig = 0.
            ambig = isfc(i) 
	      IF  ((flat (i) .GE.   -90.) .AND. &
		   (flat (i) .LE.    90.) .AND. &
		   (flon (i) .GE. -180.) .AND. &
		   (flon (i) .LE.  180.)) THEN
		IF  ( zonec )  CALL FOV2GC  ( LN, LTS, ILATEXT, &
			       flon (i), flat (i),    &
			       rain,  ambig, xorc (i), &
			       yorc (i), w (i), s (i), influc, &
			       gprtemp4c, iret )
		IF  ( zonen )  CALL FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, &
			       NNSY, flon (i), flat (i),   &
			       rain,  ambig, xorns (i), &
			       yorns (i), w (i), s (i), influn, &
			       gprtemp4n, iret )
		IF  ( zones )  CALL FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, &
			       NNSY, flon (i), flat (i),   &
			       rain,  ambig, xorns (i), &
			       yorns (i), w (i), s (i), influs, &
			       gprtemp4s, iret )
	      END IF
	    END IF
        END IF
!
	END DO
!
	RETURN
	END
