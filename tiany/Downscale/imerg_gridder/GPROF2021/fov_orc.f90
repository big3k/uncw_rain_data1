	SUBROUTINE FOV_ORC  ( flon, flat, ILATEXT, NCEDFRAC, NFOV, &
	                      NOUTER, xor, yor, iret )
!***********************************************************************
!* FOV_ORC  Find FOV orientations on a CED grid			       *
!*								       *
!* This subroutine approximates the (x,y) orientation of each FOV in a *
!* scan for a CED grid.  We just use the location of the adjacent      *
!* FOVs, except for a one-sided computation for the edge FOVs.	       *
!* NOTE: The grid is left-handed because y increases down.	       *
!*								       *
!* FOV_ORC  ( flon, flat, ILATEXT, NCEDFRAC, NFOV, NOUTER, xor, yor,   *
!*            iret )		                                       *
!*								       *
!* Input parameters:						       *
!*   flon	REAL	FOV central longitude			       *
!*   flat	REAL	FOV central latitude			       *
!*   ILATEXT	INT	starting latitude of data extent in degree     *
!*   NCEDFRAC	INT	NS grid size as gridboxes per degree	       *
!*   NFOV	INT	number of fields-of-view per scan	       *
!*   NOUTER     INT     number of edge footprints to mask (each side)  *
!*								       *
!* Output parameters:						       *
!*   xor	REAL	X component of FOV orientation in FG coord.    *
!*   yor	REAL	Y component of FOV orientation in FG coord.    *
!*   iret	INT	Return code: 0 = normal			       *
!*				    -1 = stop			       *
!**								       *
!* Log:								       *
!* G.Huffman/SSAI	07/11	Adapt amsu_grid/fov_orc.f	       *
!* G.Huffman/SSAI	11/11	Adapt FOV_ORC with LATEDGE, NFGFRAC    *
!* G.Huffman/612	06/13	Adapt amsr_nngrid/fov_orc	       *
!* E.Nelkin/SSAI	01/14	Generic version - pass in NFOV instead *
!*				of setting MXFOV in PARAMETER statement*
!* G.Huffman/612	05/16	Rename from FOV_OR		       *
!* G.Huffman/612	05/17	edge-guard footprints		       *
!* E.Nelkin/SSAI        09/17   Offset edges by NOUTER (MHS/ATMS)      *
!***********************************************************************
	IMPLICIT	NONE
	REAL*4		:: xscan, yscan
	INTEGER*4	:: nx,    ny,   nxa, nxb, nya, nyb, i, j
	INTEGER*1	:: i_d2
!
	INTEGER*4	:: ILATEXT, NCEDFRAC, NFOV, NOUTER
	REAL*4		:: WTEST,   WRAP
	REAL*4		:: flon (0:NFOV+1), flat (0:NFOV+1), &
			   xor (0:NFOV+1), yor (0:NFOV+1)
	INTEGER*4	:: iret
!
	REAL*4		:: fx0  (0:NFOV+1), fy0 (0:NFOV+1)
!
	iret = 0
	WTEST = 350. * NCEDFRAC
	WRAP  = 360. * NCEDFRAC
!
!	There is no actual gridding, so we don't need to keep longitude
!	in the range 0-360.
!
	DO  i = NOUTER, NFOV+1-NOUTER
	    fx0 (i) = flon (i) * NCEDFRAC
	    fy0 (i) = ( ILATEXT - flat (i) ) * NCEDFRAC
	END DO
!
!	Compute the X and Y orientation in grid coordinates.  It does
!	matter if neighboring FOVs jump across the lateral grid
!	wrapping boundary (the test on +/-WTEST).  The test is done
!	here so that we catch both the -180/+180 and 0/360 boundaries.
!	Do centered differences on the interior of the scan, then single-
!	sided differences at the ends, setting to missing if the data
!	aren't available.  The pixel orientation is perpendicular to the
!	line connecting the adjacent pixels, so you have to pay
!	attention to the vector properties (in left-handed coordinates):
!	   xscan yscan xor yor
!	     +     +    +   -
!	     +     -    -   -
!	     -     +    +   +
!	     -     -    -   +
!/	and you have to account for cos(lat) since we're working in
!/	units of CED grid boxes:
!/	   xor =  yscan / cos(lat)
!/	   yor = -xscan * cos(lat)
!	The range for i in actual data was (2,NFOV-1), and it stays that
!	way here.  The guard footprints are just given the first real
!       value.
!
!       SEP 2017: All lower-edge indices are increased by NOUTER, and
!       all higher-edge indices are decreased by NOUTER.
!
	DO  i = 2+NOUTER, NFOV-1-NOUTER
	    xscan   = fx0 (i+1) - fx0 (i-1)
	    IF  ( xscan .LT. -WTEST )  xscan = xscan + WRAP
	    IF  ( xscan .GT.  WTEST )  xscan = xscan - WRAP
	    yscan   = fy0 (i+1) - fy0 (i-1)
	    xor (i) =  yscan
	    yor (i) = -xscan
!	    WRITE (*,*) 'i,xscan,yscan,xor,yor', &
!			i,xscan,yscan,xor(i),yor(i)
	END DO
!
	xscan   = fx0 (2+NOUTER) - fx0 (1+NOUTER)
	IF  ( xscan .LT. -WTEST )  xscan = xscan + WRAP
	IF  ( xscan .GT.  WTEST )  xscan = xscan - WRAP
	yscan   = fy0 (2+NOUTER) - fy0 (1+NOUTER)
	xor (1+NOUTER) =  yscan
	yor (1+NOUTER) = -xscan
!
	xscan   = fx0 (1+NOUTER) - fx0 (NOUTER)
	IF  ( xscan .LT. -WTEST )  xscan = xscan + WRAP
	IF  ( xscan .GT.  WTEST )  xscan = xscan - WRAP
	yscan   = fy0 (1+NOUTER) - fy0 (NOUTER)
	xor (NOUTER) =  yscan
	yor (NOUTER) = -xscan
!
	xscan   = fx0 (NFOV-NOUTER) - fx0 (NFOV-1-NOUTER)
	IF  ( xscan .LT. -WTEST )  xscan = xscan + WRAP
	IF  ( xscan .GT.  WTEST )  xscan = xscan - WRAP
	yscan   = fy0 (NFOV-NOUTER) - fy0 (NFOV-1-NOUTER)
	xor (NFOV-NOUTER) =  yscan
	yor (NFOV-NOUTER) = -xscan
!
	xscan   = fx0 (NFOV+1-NOUTER) - fx0 (NFOV-NOUTER)
	IF  ( xscan .LT. -WTEST )  xscan = xscan + WRAP
	IF  ( xscan .GT.  WTEST )  xscan = xscan - WRAP
	yscan   = fy0 (NFOV+1-NOUTER) - fy0 (NFOV-NOUTER)
	xor (NFOV+1-NOUTER) =  yscan
	yor (NFOV+1-NOUTER) = -xscan
!
	RETURN
	END
