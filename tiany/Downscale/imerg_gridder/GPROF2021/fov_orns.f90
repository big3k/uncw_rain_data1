	SUBROUTINE FOV_ORNS  ( flon, flat, NNSX, NNSY, NNSFRAC, &
			       NFOV, NOUTER, xor, yor, iret )
!***********************************************************************
!* FOV_ORNS  Find FOV orientations on the north, south PTP grids       *
!*								       *
!* This subroutine approximates the (x,y) orientation of each FOV in a *
!* scan for the Polar Tangent Plane north and south grids.  We just    *
!* use the location of the adjacent FOVs, except for a one-sided       *
!* computation for the edge FOVs.				       *
!* NOTE: The routine makes no checks that the latitudes are appropriate*
!* to the hemisphere's grids being passed in.			       *
!* NOTE: The grid is left-handed because y increases down.	       *
!*								       *
!* FOV_ORNS  ( flon, flat, NNSX, NNSY, NNSFRAC, NFOV, NOUTER, xor, yor,*
!*             iret )			                               *
!*								       *
!* Input parameters:						       *
!*   flon	REAL	FOV central longitude			       *
!*   flat	REAL	FOV central latitude			       *
!*   NNSX	INT	X size of Polar Tangent Planes		       *
!*   NNSY	INT	Y size of Polar Tangent Planes		       *
!*   NNSFRAC	INT	NS grid size as gridboxes per degree	       *
!*   NFOV	INT	number of footprints			       *
!*   NOUTER     INT     number of edge footprints to mask (each side)  *
!*								       *
!* Output parameters:						       *
!*   xor	REAL	X component of FOV orientation in PTP coord.   *
!*   yor	REAL	Y component of FOV orientation in PTP coord.   *
!*   iret	INT	Return code: 0 = normal			       *
!*				    -1 = stop			       *
!**								       *
!* Log:								       *
!* G.Huffman/SSAI	09/03	Adapt FOV_ORC.F			       *
!* G.Huffman/612	06/16	Adapt AMSU_GRID/FOV_ORNS.F	       *
!* G.Huffman/612	05/17	edge-guard footprints		       *
!* E.Nelkin/SSAI        09/17   First DO loop now 1, NFOV; account for *
!*                              possible missing lat/lon; add FILLCHK  *
!* E.Nelkin/SSAI        09/17   Offset edges by NOUTER (MHS/ATMS)      *
!***********************************************************************
	IMPLICIT	NONE
	INTEGER*4	:: i, j
	REAL*4		:: hemi, FILLCHK
        PARAMETER       ( FILLCHK = -8888. )
!
	INTEGER*4	:: NNSX, NNSY, NNSFRAC, NFOV, NOUTER
	CHARACTER	:: zone
	REAL*4		:: flon (0:NFOV+1), flat (0:NFOV+1), &
			   xor (0:NFOV+1), yor (0:NFOV+1)
	INTEGER*4	:: iret
!
	REAL*4		:: fx0 (0:NFOV+1), fy0 (0:NFOV+1)
!
	iret = 0
!
!	Unlike FOV2GC, there is no actual gridding, so we don't need
!	to keep longitude in the range 0-360.
!	Unlike FOV_ORC, we aren't working on the PPS transposed grid,
!	so X and Y are as expected.
!
!       For NOUTER = 0 (all instruments other than MHS/ATMS), loop
!       indices effectively changed to 1, NFOV because we do not really
!       trust the lat/lon values at the edge footprints 0, NFOV+1
!       that were calculated via extrapolation in ADDMSCAN/GETGPROFG.
!       For NOUTER > 0 (MHS, ATMS), the lat/lon values have been set
!       to missing (-9999.) by CSU, so looping from 1+NOUTER to
!       NFOV-NOUTER is necessary.
!
        DO  i = 1+NOUTER, NFOV-NOUTER
            IF  ( flat (i) .GT. FILLCHK .AND. &
                  flon (i) .GT. FILLCHK ) THEN
	        IF  ( flat (i) .GE. 0. )  THEN
		    hemi = 1.
	        ELSE
		    hemi = -1.
	        END IF
	        fx0 (i) = NNSX * 0.5 + ( 90. - hemi * flat (i) ) *   &
				   COSD (hemi * flon (i)) * NNSFRAC
	        fy0 (i) = NNSY * 0.5 - ( 90. - hemi * flat (i) ) *   &
				   SIND (hemi * flon (i)) * NNSFRAC
           ELSE
                fx0 (i) = 0.
                fy0 (i) = 0.
           END IF
	END DO
!
!	Compute the X and Y orientation in PTP grid coordinates.
!	Like FOV2FGC, there is no Prime Meridian jump in X.  Do
!	centered diff!erences on the interior of the scan, then
!	single-sided differences at the ends.  The range for i in
!	actual data was (2,NFOV-1), and it stays that way here.
!	The guard footprints are just given the first real value.
!
!       Graphically, where X = legitimate FOV, O = fictional:
!          1  2  3      NFOV-2 NFOV-1 NFOV
!       O  X  X  X .....  X      X      X      O
!
!       Assume that whole scans are bad if any lat or lon value
!       is missing, rather than attempting to devise intricate
!       code to account for all possibilities of isolated
!       footprints with missing navigation.
!
!       SEP 2017: All lower-edge indices are increased by NOUTER, and
!       all higher-edge indices are decreased by NOUTER.
!
	DO  i = 2+NOUTER,NFOV-1-NOUTER
	    xor (i) = fx0 (i+1) - fx0 (i-1)
	    yor (i) = fy0 (i+1) - fy0 (i-1)
	END DO
	xor (1+NOUTER) = fx0 (2+NOUTER) - fx0 (1+NOUTER)
	yor (1+NOUTER) = fy0 (2+NOUTER) - fy0 (1+NOUTER)
	xor (NFOV-NOUTER) = fx0 (NFOV-NOUTER) - fx0 (NFOV-1-NOUTER)
	yor (NFOV-NOUTER) = fy0 (NFOV-NOUTER) - fy0 (NFOV-1-NOUTER)
!
	xor (NOUTER) = xor (1+NOUTER)
	yor (NOUTER) = yor (1+NOUTER)
	xor (NFOV+1-NOUTER) = xor (NFOV-NOUTER)
	yor (NFOV+1-NOUTER) = yor (NFOV-NOUTER)
!
	RETURN
	END
