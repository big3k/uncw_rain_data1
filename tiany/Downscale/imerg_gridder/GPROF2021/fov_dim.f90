	SUBROUTINE FOV_DIM  ( NFOV, instrument, w, s, iret )
!************************************************************************
!* FOV_DIM  Compute the dimensions of FOVs for a scan			*
!*									*
!* This subroutine approximates the FOV sizes in a scan.  For sounders, *
!* the geometry is based on program fov_B.c, sent by Ralph Ferraro	*
!* (NESDIS/ORA) on 9/8/03.  For imagers, values were provided by Wes    *
!* Berg of Colorado State University on 2/3/15.				*
!*									*
!* FOV_DIM  ( NFOV, instrument, w, s, iret )				*
!*									*
!* Input parameters:							*
!*   NFOV	INT	Number of fields-of-view per scan		*
!*   instrument CHAR*	name of instrument				*
!*									*
!* Output parameters:							*
!*   w		REAL	FOV width (cross-scan) in km (at 21-23 GHz)	*
!*   s		REAL	FOV length (along-scan) in km (at 21-23 GHz)	*
!*   iret	INT	Return code: 0 = normal				*
!*				    -1 = stop				*
!**									*
!* Log:									*
!* G.Huffman/SSAI	07/11	Adapt ssmi_nngrid/fov_dim.f		*
!* G.Huffman/612	06/13	Adapt amsr_nngrid/fov_dim.f		*
!* E.Nelkin/SSAI	01/14	Generic version - pass in NFOV instead	*
!*				of setting MXFOV in PARAMETER statement	*
!* E.Nelkin/SSAI	01/14	Pass in instrument; compute w and s for	*
!*				AMSU, MHS and hard-code for others	*
!* E.Nelkin/SSAI	02/15	Correct w, s values for imagers; add	*
!*				computations of w, s for ATMS, SAPHIR	*
!* G.Huffman/612	05/17	edge-guard footprints		        *
!* E.Nelkin/SSAI        08/18   Update w, s values for 19 GHz resolution*
!************************************************************************
	IMPLICIT	NONE
	INTEGER*4	:: i
	REAL*4		:: sc_ang, s_ec2s, co_ec2s
	REAL*8		:: xx, xy, yy, xor2, xoryor2, yor2, hor2s2, &
			   hor2w2
!
	INTEGER*4	:: LATEDGE, NFGFRAC, NCX, NCY, NFOV
	CHARACTER*256	:: instrument
	REAL*4		:: RE, SZDEG, SCANINC, RSAT
	PARAMETER	( LATEDGE = 75, &
			  NFGFRAC = 16, &
			  NCX     = 360     * NFGFRAC, &
			  NCY     = LATEDGE * NFGFRAC * 2, &
			  RE      = 6371., &
			  SZDEG   = 3.1415926 * 2. * RE / 360. )
!
! WARNING: SCANINC and RSAT are needed for w, s calculations for the
! sounders.  Previously, they were listed in the above PARAMETER
! statement.  However, with multiple sounder instruments, each with
! different values, these have been defined within the individual
! IF tests below.
!
	REAL*4		:: w (0:NFOV+1), s (0:NFOV+1)
	INTEGER*4	:: iret
!
	REAL*4		:: e_a (0:NFOV+2), d (0:NFOV+2)
!
!ZZZZZSTRT
!	WRITE (*, *) 'i, sc_ang, s_cozen, ang11, e_a (i), d (i)'
!ZZZZZSTOP
	iret = 0
!
!	Calculate w and s for sounders - MHS, AMSU, ATMS, and SAPHIR.
!	For other instruments, hard-code these values.
!
!  Note: On August 8, 2018, Dave Randel provided the following
!  values.  We decided to stick with our calculations, which are
!  not identical, but similar.
!
!  XTrack:  10 different footprint sizes for each sensor depending
!  on scan position:
!  MHS, ATMS scan edge                                          nadir
!  downtrack:
!  27.712,24.690,22.042,20.170,18.812,17.821,17.111,16.631,16.348,16.249
!  crosstrack:
!  54.107,41.082,31.671,25.980,22.288,19.818,18.158,17.083,16.767,16.245
!
!  AMSUB scan edge                                              nadir
!  downtrack:
!  28.391,25.257,22.523,20.595,19.198,18.180,17.452,16.960,16.669,16.563
!  crosstrack:
!  55.962,42.283,32.483,26.586,22.780,20.235,18.528,17.424,16.792,16.564
!
	IF ( TRIM(instrument) .EQ. 'MHS' .OR. &
	     TRIM(instrument) .EQ. 'AMSUB' ) THEN
!
!	sc_ang  = scan angle (measured from nadir, in deg)
!	s_ec2s  = sine of Earth-center-to-satellite angle at FOV
!	co_ec2s = 180deg - Earth-center-to-satellite angle at FOV (ATAN
!		  returns in the interval [-90.90], but ec2s is > 90deg)
!	e_a     = Earth angle from satellite to FOV in deg
!	d       = satellite-to-FOV distance in km
!
! Values of SCANINC and RSAT obtained from http://mirs.nesdis.noaa.gov/
! mhs.php.  (See also http://oiswww.eumetsat.org/WEBOPS/eps-pg/ATOVS-L1/
! ATOVSL1-PG-4ProdOverview.htm) A web search reveals that RSAT varies
! within a range of ~817-870 km for the various MHS satellites (NOAA-18,
! NOAA-19, MetOp-A, and MetOp-B).
! The actual data require i = 1, NFOV+1; the extra footprint at each
! end is for the missing-filled guard footprints.
!
	   SCANINC = 1.111
	   RSAT = 833.
!
	   DO  i = 0, NFOV+2
		sc_ang  = SCANINC * ( i - 1 - NFOV / 2 )
		s_ec2s  = ( RE + RSAT ) * SIND ( sc_ang ) / RE
		co_ec2s = ATAND ( s_ec2s / SQRT ( 1. - s_ec2s*s_ec2s ) )
		e_a (i) = co_ec2s - sc_ang
		IF  ( sc_ang .EQ. 0. )  THEN
		    d (i) = RSAT
		ELSE
		    d (i) = RE * SIND ( e_a (i) ) / SIND ( sc_ang )
		END IF
		IF  ( i .GT. 0 )  THEN
		    s (i-1) = ABS ( e_a (i) - e_a (i-1) ) * RE      * &
			      3.1415926 / 180.
		    w (i-1) = ( d (i-1) + d (i) ) * 0.5   * SCANINC * &
			      3.1415926 / 180.
!ZZZZZSTRT
!		WRITE (*, *) i, sc_ang, s_cozen, ang11, e_a (i), d (i)
!ZZZZZSTOP
		END IF
	   END DO
	ELSE IF ( TRIM(instrument) .EQ. 'ATMS'  ) THEN
!
! RSAT value from:
! https://directory.eoportal.org/web/eoportal/satellite-missions/s/suomi-npp
! SCANINC value from:
! http://asl.umbc.edu/pub/reports/npp_nasa_hq_report.pdf
!
	   SCANINC = 1.11
	   RSAT = 824.
!
	   DO  i = 0, NFOV+2
		sc_ang  = SCANINC * ( i - 1 - NFOV / 2 )
		s_ec2s  = ( RE + RSAT ) * SIND ( sc_ang ) / RE
		co_ec2s = ATAND ( s_ec2s / SQRT ( 1. - s_ec2s*s_ec2s ) )
		e_a (i) = co_ec2s - sc_ang
		IF  ( sc_ang .EQ. 0. )  THEN
		    d (i) = RSAT
		ELSE
		    d (i) = RE * SIND ( e_a (i) ) / SIND ( sc_ang )
		END IF
		IF  ( i .GT. 0 )  THEN
		    s (i-1) = ABS ( e_a (i) - e_a (i-1) ) * RE      * &
			      3.1415926 / 180.
		    w (i-1) = ( d (i-1) + d (i) ) * 0.5   * SCANINC * &
			      3.1415926 / 180.
		END IF
	   END DO
	ELSE IF ( TRIM(instrument) .EQ. 'SAPHIR' ) THEN
!
! RSAT value from:
! https://directory.eoportal.org/web/eoportal/satellite-missions/m/
! megha-tropiques
! SCANINC value from:
! http://meghatropiques.ipsl.polytechnique.fr/dmdocuments/proc_s3p02.pdf,
! with subsequent calculation SCANINC = 42 degrees / (NFOV/2), where
! NFOV = 182, leading to SCANINC = 0.46 degrees.
!
	   SCANINC = 0.46
	   RSAT = 865.
!
	   DO  i = 0, NFOV+2
		sc_ang  = SCANINC * ( i - 1 - NFOV / 2 )
		s_ec2s  = ( RE + RSAT ) * SIND ( sc_ang ) / RE
		co_ec2s = ATAND ( s_ec2s / SQRT ( 1. - s_ec2s*s_ec2s ) )
		e_a (i) = co_ec2s - sc_ang
		IF  ( sc_ang .EQ. 0. )  THEN
		    d (i) = RSAT
		ELSE
		    d (i) = RE * SIND ( e_a (i) ) / SIND ( sc_ang )
		END IF
		IF  ( i .GT. 0 )  THEN
		    s (i-1) = ABS ( e_a (i) - e_a (i-1) ) * RE      * &
			      3.1415926 / 180.
		    w (i-1) = ( d (i-1) + d (i) ) * 0.5   * SCANINC * &
			      3.1415926 / 180.
		END IF
	   END DO
	ELSE IF ( TRIM(instrument) .EQ. 'AMSR2' ) THEN
	   DO  i = 0, NFOV+1
		s (i) = 22.0
		w (i) = 14.0
	   END DO
	ELSE IF ( TRIM(instrument) .EQ. 'AMSRE' ) THEN
	   DO  i = 0, NFOV+1
		s (i) = 27.0
		w (i) = 16.0
	   END DO
	ELSE IF ( TRIM(instrument) .EQ. 'GMI' ) THEN
	   DO  i = 0, NFOV+1
		s (i) = 18.1
		w (i) = 10.9
	   END DO
	ELSE IF ( TRIM(instrument) .EQ. 'MADRAS' ) THEN
! Currently MADRAS is unusable; leave default values as is.
	   DO  i = 0, NFOV+1
		s (i) = 16
		w (i) = 14
	   END DO
	ELSE IF ( TRIM(instrument) .EQ. 'SSMI' ) THEN
	   DO  i = 0, NFOV+1
		s (i) = 69.0
		w (i) = 43.0
	   END DO
	ELSE IF ( TRIM(instrument) .EQ. 'SSMIS' ) THEN
	   DO  i = 0, NFOV+1
		s (i) = 74.0
		w (i) = 45.0
	   END DO
	ELSE IF ( TRIM(instrument) .EQ. 'TMI' ) THEN
	   DO  i = 0, NFOV+1
!
! Use post-boost (402 km) values, since this constitutes the majority of
! the record (August 22, 2001 to about mid-2014).  Pre-boost (350 km)
! values are s = 30.3, w = 18.3, per Dave Randel.  TRMM began descending
! during 2014, reaching pre-boost altitude around March 2015.
!
		s (i) = 34.6
		w (i) = 20.9
	   END DO
	END IF
!
	RETURN
	END
