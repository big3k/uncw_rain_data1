	SUBROUTINE FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, NNSY, &
			      flon, flat, fp, fa, xor, yor, w, s, &
			      influ, gprtemp, iret )
!***********************************************************************
!* FOV2GNS  Map a FOV to the north and south PTP grids		       *
!*								       *
!* This subroutine maps one FOV to corresponding grid boxes in the     *
!* regular north and south Polar Tangent Plane grids.  The pixel value *
!* is attributed to the box when the box is closer to the pixel than   *
!* the box's current pixel, where "close" is defined as squared	       *
!* elliptical distance from pixel center.			       *
!* NOTE: The routine makes no checks that the latitudes are appropriate*
!* to the hemisphere's grids being passed in.			       *
!* NOTE: The grid is left-handed because y increases down.	       *
!*								       *
!* FOV2GNS  ( ILATEXT, NNSFRAC, NNSX, NNSY, flon, flat, fp, fa,        *
!*            xor, yor, w, s, influ, gprtemp, iret )		       *
!*								       *
!* Input parameters:						       *
!*   ILATEXT	INT	starting latitude of data extent in degrees    *
!*			(assume symmetrical)			       *
!*   NNSFRAC	INT	NS grid size as gridboxes per degree	       *
!*   NNSX	INT	X size of north and south PTP		       *
!*   NNSY	INT	Y size of north and south PTP		       *
!*   flon	REAL	FOV central longitude			       *
!*   flat	REAL	FOV central latitude			       *
!*   fp		REAL	FOV precip				       *
!*   fa		REAL	FOV ambiguous				       *
!*   xor	REAL	X component of FOV orientation in FG coord.    *
!*   yor	REAL	Y component of FOV orientation in FG coord.    *
!*   w		REAL	FOV width (cross-scan) in km		       *
!*   s		REAL	FOV length (along-scan) in km		       *
!*								       *
!* Input and output parameters:					       *
!*   influ	INT1*	Fine-grid elliptical distance squared, I1      *
!*			scaled by 10				       *
!*   gprtemp	REAL4*	Work array size (NNSX,NNSY,4) [note that the   *
!*			index order is PTPx,PTPy,variables].  The      *
!*			variables are:				       *
!*			  precip (in mm/hr)			       *
!*			  number of footprints (0 or 1)		       *
!*			  number of raining footprints (0 or 1)	       *
!*			  number of ambiguous footprints (0 or 1)      *
!*								       *
!* Output parameters:						       *
!*   iret	INT	Return code: 0 = normal			       *
!*				    -1 = stop			       *
!**								       *
!* Log:								       *
!* G.Huffman/SSAI	 9/03	Adapt FOV2FGC.F			       *
!* G.Huffman/SSAI	12/03	Delete wrapping at lateral edges       *
!* G.Huffman/SSAI	 5/04	Document no need for lon=lat=0. check  *
!* G.Huffman/612	05/16	Adapt AMSU_GRID/FOV2FGNS	       *
!***********************************************************************
	IMPLICIT	NONE
	INTEGER*4	:: nxmx, nymx, nx0, ny0, nxa, nxb, nya, nyb, &
			   i, j
	INTEGER*1	:: i_d2
	REAL*4		:: fx0, fy0, x, y, d2, w_fg, s_fg, hemi
	REAL*8		:: xx, xy, yy, xor2, xoryor2, yor2, hor2s2, &
			   hor2w2
!
	INTEGER*4	:: ILATEXT, NNSFRAC, NNSX, NNSY
	REAL*4		:: RE, SZDEG
	PARAMETER	( RE    = 6371.,			&
			  SZDEG = 3.1415926 * 2. * RE / 360. )
	REAL*4		:: flon, flat, fp, fa, xor, yor, w, s, &
			   gprtemp(NNSX, NNSY, 4)
	INTEGER*1	:: influ(NNSX, NNSY)
	INTEGER*4	:: iret
!
	iret = 0
!
!	No check for lon=lat=0. is needed because this routine is only
!	called for high latitudes.
!
!	As in FOV2GC, both the orientation and the PTP computation
!	are in y-increasing-down left-handed coordinate
!	systems, so where we use directed pairs (x,y), the negative of
!	the y is used.  w and s are converted to units of PTP boxes.
!
	IF  ( flat .GE. 0. )  THEN
	    hemi = 1
	  ELSE
	    hemi = -1
	END IF
	fx0 = NNSX * 0.5 +					&
	      ( 90. - hemi * flat ) * COSD (hemi * flon) * NNSFRAC
	fy0 = NNSY * 0.5 -					&
	      ( 90. - hemi * flat ) * SIND (hemi * flon) * NNSFRAC
	nx0     = fx0 + 1.
	ny0     = fy0 + 1.
	nxmx    = s / SZDEG * NNSFRAC
	nymx    = s / SZDEG * NNSFRAC
	w_fg    = w / SZDEG * NNSFRAC
	s_fg    = s / SZDEG * NNSFRAC
	xor2    =  xor * xor
	xoryor2 =  xor * -yor * 2
	yor2    = -yor * -yor
	hor2s2  = 4. / ( xor2 + yor2 ) / ( s_fg * s_fg )
	hor2w2  = 4. / ( xor2 + yor2 ) / ( w_fg * w_fg )
!
!	Set limits and check that some of the FOV is on the grid.
!	Unlike the central grid, there is no wrapping at the boundaries
!	because this is just a regional grid.  If there are resulting
!       gaps in the influence field, the 0.7 factor can be increased.
!
	nxa = nx0 - nxmx * 0.7 - 1
	nxb = nx0 + nxmx * 0.7 + 1
	nya = ny0 - nymx * 0.7 - 1
	nyb = ny0 + nymx * 0.7 + 1
	IF  ( nxb .LT. 1 .OR. nxa .GT. NNSX )  RETURN
	IF  ( nxa .LT. 1   )  nxa = 1
	IF  ( nxb .GT. NNSX  )  nxb = NNSX
	IF  ( nyb .LT. 1 .OR. nya .GT. NNSY )  RETURN
	IF  ( nya .LT. 1   )  nya = 1
	IF  ( nyb .GT. NNSY  )  nyb = NNSY
!
!	Loop through PTP grid boxes, computing squared elliptical
!	distance, and inserting pixel values if the distance is smaller
!	than the present grid value.
!
!	The squared elliptical distance is
!	d2 = (x'/(s/2))**2 + (y'/(w/2))**2
!	where x',y' are distances from FOV center along- and across-
!	scan direction, and d2=1 defines the nominal edge of the FOV.
!	The rotated x',y' coordinate system is defined by
!	x' = R * COS (theta-theta0)
!	y' = R * SIN (theta-theta0)
!	with R      = common radial coordinate value,
!	     theta  = angle in map x,y system,
!	     theta0 = angle from x to x', defined by xor,yor, and
!	     the origins are co-located.
!	Substitute these x',y' expressions into d2 and expand.  The
!	basic product terms are pre-computed for speed.
!
!test STRT
!	WRITE (*,*) 'fx0,fy0,nx0,ny0,nxmx,nymx,nxa,nxb,nya,nyb,xor,yor',
!     + 	    fx0,fy0,nx0,ny0,nxmx,nymx,nxa,nxb,nya,nyb,xor,yor
!	WRITE (*, *) 'x,y,d2,i_d2'
!test STOP
	DO  j = nya, nyb
	DO  i = nxa, nxb
	    x = ( FLOAT (i) - 0.5 ) - fx0
	    y = ( FLOAT (j) - 0.5 ) - fy0
	    xx =  x * x
	    xy =  x * -y
	    yy = -y * -y
	    d2 = ( xx * xor2 + xy * xoryor2 + yy * yor2 ) * hor2s2 + &
		 ( xx * yor2 - xy * xoryor2 + yy * xor2 ) * hor2w2
	    i_d2 = AMIN1 ( 50., 10. * d2 )
	    IF  ( i_d2 .LT. influ (i, j) )  THEN
		influ (i, j) = i_d2
		gprtemp (i, j, 1) = fp
		gprtemp (i, j, 2) = 1
		IF  ( NINT(100.*fp) .GT. 0 )  THEN
		    gprtemp (i, j, 3) =  1.
		  ELSE
		    gprtemp (i, j, 3) =  0.
		END IF
		gprtemp (i, j, 4) = fa
	    END IF
!test STRT
!	    WRITE (*, *) x,y,d2,i_d2
!test STOP
!test2 STRT
!	    IF  ( i .EQ. 1 )  THEN
!		WRITE (*,*) 'i,j,fx0,fy0,nx0,ny0,nxmx,nymx,nxa,nxb,nya,nyb,xor,yor', &
!		i,j,fx0,fy0,nx0,ny0,nxmx,nymx,nxa,nxb,nya,nyb,xor,yor
!		WRITE (*, *) 'x,y,d2,i_d2'
!	    	WRITE (*, *) x,y,d2,i_d2
!		STOP
!	    END IF
!test2 STOP
	END DO
	END DO
!test STRT
! enable this code to test the first scan-edge pixel
!	OPEN (17, FILE = 'dump.influn11', FORM = 'UNFORMATTED')
!	WRITE (17) influ
!	CLOSE (17)
!	IF  ( i .GE. 1 )  STOP
!test STOP
!
	RETURN
	END
