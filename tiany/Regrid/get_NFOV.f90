! cut out of L2AGPROF2021_nngrid.f90
!

       subroutine get_NFOV(instrument, NFOV, scans_per_orbit, & 
                            rate_thresh) 
!***********************************************************************
!      The latitude/longitude resolution and latitude offsets are
!      specified in parm_res.h.
!
      IMPLICIT    NONE

      INCLUDE "parm_res.h"
!
      CHARACTER*256      :: instrument
      INTEGER*4      :: NFOV, scans_per_orbit
      real*4      :: rate_thresh

!* Input parameters:                                        *
!*   instrument CHAR*   name of instrument                        *
!*                                                    *
!* Output parameters:                                       *
!*   NFOV   INT   Number of fields-of-view per scan         *
!*   scans_per_orbit   INT   Number of fields-of-view per scan         *
!*   rate_thresh    real    rainrate threshold

!
!***********************************************************************
!      Set NFOV and scans_per_orbit based on the instrument name.
!      Values obtained by adding 3 to the max scans reported by
!      Joyce Chou in 4/9/14 e-mail.  Assumptions are made that the
!      value for AMSRE is like AMSR2, AMSUB is like MHS, and SSMI is
!      like SSMIS.  The value for MADRAS came from John Stout in
!      12/19/13 e-mail.
!
!      2/11/2015: Scale the original values by 75%.  For sensors whose
!      orbit break is at high (northern) latitudes, the fanning out of
!      the grid boxes in an east-west direction caused overlap between
!      grid boxes at the end of the orbit and those from the beginning
!      of the orbit.  Reducing scans_per_orbit will prevent this, at
!      the expense of usually producing two output gridded files,
!      instead of one, per input file.
!
!      8/04/2016: Implement rate_thresh.  Rates below this value
!      will be ignored in ADDMSCAN.  Initially, set this to 0.03 mm/h
!      for GMI, based on an analysis by Chris Kidd of both POP and
!      precip rate thresholds by sensor and month.  Set the value to
!      zero for all other sensors, and let the IMERG calibration code
!      intercalibrate them against GMI.
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
!!         scans_per_orbit = 3960
         rate_thresh = 0.03
!!         rate_thresh = 0.05
      ELSE IF ( TRIM(instrument) .EQ. 'AMSRE' ) THEN
         NFOV = 392
         scans_per_orbit = 2970
!!         scans_per_orbit = 3960
         rate_thresh = 0.03
!!         rate_thresh = 0.05
      ELSE IF ( TRIM(instrument) .EQ. 'AMSUB' ) THEN
         NFOV = 90
         scans_per_orbit = 1725
!!         scans_per_orbit = 2300
         rate_thresh = 0.03
!!         rate_thresh = 0.12
      ELSE IF ( TRIM(instrument) .EQ. 'ATMS' ) THEN
         NFOV = 96
         scans_per_orbit = 1716
!!         scans_per_orbit = 2289
         rate_thresh = 0.03
!!         rate_thresh = 0.12
      ELSE IF ( TRIM(instrument) .EQ. 'GMI' ) THEN
         NFOV = 221
         scans_per_orbit = 2224
!!         scans_per_orbit = 2966
!!      rate_thresh = 0.00
           rate_thresh = 0.03
!!      ELSE IF ( TRIM(instrument) .EQ. 'MADRAS' ) THEN
!!         NFOV = 960
!!         scans_per_orbit = 1850
!!         scans_per_orbit = 2467
!!         rate_thresh = 0.
      ELSE IF ( TRIM(instrument) .EQ. 'MHS' ) THEN
         NFOV = 90
         scans_per_orbit = 1725
!!         scans_per_orbit = 2300
         rate_thresh = 0.03
!!         rate_thresh = 0.12
      ELSE IF ( TRIM(instrument) .EQ. 'SAPHIR' ) THEN
         NFOV = 182
         scans_per_orbit = 2803
!!         scans_per_orbit = 3738
         rate_thresh = 0.03
!!         rate_thresh = 0.12
      ELSE IF ( TRIM(instrument) .EQ. 'SSMI' ) THEN
! High-frequency (85 GHz) value:
         NFOV = 128
         scans_per_orbit = 2420
!!         scans_per_orbit = 3226
         rate_thresh = 0.03
!!         rate_thresh = 0.06
      ELSE IF ( TRIM(instrument) .EQ. 'SSMIS' ) THEN
         NFOV = 180
         scans_per_orbit = 2420
!!         scans_per_orbit = 3226
         rate_thresh = 0.03
!!         rate_thresh = 0.06
      ELSE IF ( TRIM(instrument) .EQ. 'TMI' ) THEN
         NFOV = 208
         scans_per_orbit = 2192
!!         scans_per_orbit = 2924
         rate_thresh = 0.03
      END IF

      Return
      End
