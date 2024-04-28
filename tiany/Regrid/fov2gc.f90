        SUBROUTINE FOV2GC  ( LN, LTS, ILATEXT, flon, flat,  fp,  &
                             fa, xor, yor, w,  s,    influ, gprtemp, &
                             iret )
!************************************************************************
!* FOV2GC  Map a FOV to the central CED grid                                *
!*                                                                        *
!* This subroutine maps one FOV to corresponding grid boxes in the        *
!* central CED grid.  The pixel value is attributed to the box when the        *
!* box is closer to the pixel than the box's current pixel, where        *
!* "close" is defined as squared elliptical distance from pixel center.        *
!* NOTE: The grid is left-handed because y increases down.                *
!*                                                                        *
!* FOV2GC  ( LN, LTS, ILATEXT, flon, flat, fp, fa, xor, yor, w, s,        *
!*           influ,   gprtemp,  iret )                                        *
!*                                                                        *
!* Input parameters:                                                        *
!*   LN                INT4        Number of X grid boxes (in [-180,180])                *
!*   LTS        INT4        Number of Y grid boxes (in [-70,70])                *
!*   ILATEXT        INT4        Degrees of lat. in one hemisphere                *
!*   flon        REAL        FOV central longitude                                *
!*   flat        REAL        FOV central latitude                                *
!*   fp                REAL        FOV precip                                        *
!*   fa                REAL        FOV ambiguous flag                                *
!*   xor        REAL        X component of FOV orientation in grid coord.        *
!*   yor        REAL        Y component of FOV orientation in grid coord.        *
!*   w                REAL        FOV width (cross-scan) in km                        *
!*   s                REAL        FOV length (along-scan) in km                        *
!*                                                                        *
!* Input and output parameters:                                                *
!*   influ        INT1*        Grid elliptical distance squared, I1; scaled so        *
!*                        half-power point = 10                                *
!*   gprtemp        REAL4*        Work array size (LTS,LN,4) [note that the        *
!*                        index order is lat,lon,variables].  The                *
!*                        variables are:                                        *
!*                          precip (in mm/hr)                                *
!*                          number of footprints (0 or 1)                        *
!*                          number of raining footprints (0 or 1)                *
!*                          number of ambiguous footprints (0 or 1)        *
!*                                                                        *
!* Output parameters:                                                        *
!*   iret        INT        Return code: 0 = normal                                *
!*                                    -1 = stop                                *
!**                                                                        *
!* Log:                                                                        *
!* G.Huffman/SSAI        07/11        Adapt amsu_grid/fov2fgc.f, add ambiguous*
!* G.Huffman/612        06/13        Adapt amsr_grid/fov2g.f; load tmitemp        *
!* E.Nelkin/SSAI        11/13        Reduce tmitemp to 4 by previously        *
!*                                loading time in fov2g_timeonly                *
!* E.Nelkin/SSAI         1/14        Revise test for # of raining pixels:        *
!*                                set to 1 only if NINT(100*rain) > 0        *
!* G.Huffman/612        05/16        Adapt FOV2G                                *
!* E.Nelkin/SSAI        09/17   Revise nxa, nxb, nya, nyb calculations  *
!************************************************************************
        IMPLICIT        NONE
        INTEGER*4        :: nxmx, nymx, nx0, ny0,  nxa,     nxb,  nya, &
                           nyb,  i,    ii,  j,    nxpps,   nypps,     &
                           ipps, jpps
        INTEGER*1        :: i_d2
        REAL*4                :: fx0,  fy0,  x,   y,    d2,      w_fg, s_fg, &
                           cosl, alnfac,    altfac
        REAL*8                :: xx,   xy,   yy,  xor2, xoryor2, yor2, &
                           hor2s2, hor2w2
!
        REAL*4                :: RE,   SZDEG
        PARAMETER        ( RE      = 6371., &
                          SZDEG   = 3.1415926 * 2. * RE / 360. )
        REAL*4                :: flon, flat, fp, fa, xor, yor, w, s, &
                           gprtemp (LTS, LN, 4)
        INTEGER*1        :: influ (LN, LTS)
        INTEGER*4        :: LN, LTS, ILATEXT, iret
!
        iret   = 0
!
!        First, check if (lon,lat) = (0,0) or (180,0), which essentially
!        always marks an error in navigation.  If so, just return.
!
        IF  ( ( flon .EQ. 0. .OR. flon .EQ. 180. ) .AND. &
               flat .EQ. 0. )  RETURN
!
!        Both the orientation and the nx0|ny0-based grid computation
!        are in y- increasing-down left-handed coordinate systems,
!        so where we use directed pairs (x,y), the negative of the y
!        is used.  xor,yor are in grid coordinates, so xor must be
!        scaled by 1/cosl to work in grid lat. boxes.
!
!        W and S are converted to units of grid lat. boxes.
!
!        The footprint center (nx0,ny0) is taken from the previous
!        addmscan code.  The factors [alnfac,altfac] assign the
!        traditional top left of the data at [lon,lat] = [-180,70]
!        to grid location [x,y] = [1,801].  fx0|fy0 and nx0|ny0 keep
!        the code in the original left-hand coordinate system to avoid
!        a long development cycle in the new system.
!
        alnfac = 180. + (360. / FLOAT (LN))
        altfac = FLOAT (ILATEXT) + (360. / FLOAT (LN))
        nxpps  = INT ( (alnfac + flon) * FLOAT (LN / 360) )
        IF  ( nxpps .EQ. (LN  + 1) )  nxpps = LN
        nypps  = INT ( (altfac + flat) * FLOAT (LN / 360) )
        IF  ( nypps .EQ. (LTS + 1) )  nypps = LTS
        fx0    = (alnfac + flon) * FLOAT (LN / 360)
        fy0    = LTS + 1 - (altfac + flat) * FLOAT (LN / 360)
        nx0    = nxpps
        ny0    = LTS + 1 - nypps
!
        cosl    = COSD ( flat )
        nxmx    = s / SZDEG * ( LN / 360. ) / cosl
        nymx    = s / SZDEG * ( LN / 360. )
        w_fg    = w / SZDEG * ( LN / 360. )
        s_fg    = s / SZDEG * ( LN / 360. )
        xor2    =  xor *  xor * ( cosl * cosl )
        xoryor2 =  xor * -yor * cosl * 2
        yor2    = -yor * -yor
        hor2s2  = 4. / ( xor2 + yor2 ) / ( s_fg * s_fg )
        hor2w2  = 4. / ( xor2 + yor2 ) / ( w_fg * w_fg )
!
!        Set limits and check that some of the FOV is on the grid.
!       If there are resulting gaps in the influence field, the 0.7
!       factor can be increased.
!
        nxa = nx0 - nxmx * 0.7 - 1
        nxb = nx0 + nxmx * 0.7 + 1
        nya = ny0 - nymx * 0.7 - 1
        nyb = ny0 + nymx * 0.7 + 1
        IF  ( nyb .LT. 1 .OR. nya .GT. LTS )  RETURN
        IF  ( nya .LT. 1   )  nya = 1
        IF  ( nyb .GT. LTS )  nyb = LTS
!
!        Loop through CED grid boxes, wrapping at the lateral grid
!        edges, if needed, computing squared elliptical distance, and
!        inserting pixel values if the distance is smaller than the
!        present grid value.
!
!        The squared elliptical distance is
!        d2 = (x'/(s/2))**2 + (y'/(w/2))**2
!        where x',y' are distances from FOV center along- and across-
!        scan direction, and d2=1 defines the nominal edge of the FOV.
!        The rotated x',y' coordinate system is defined by
!        x' = R * COS (theta-theta0)
!        y' = R * SIN (theta-theta0)
!        with R      = common radial coordinate value,
!             theta  = angle in map x,y system,
!             theta0 = angle from x to x', defined by xor,yor, and
!             the origins are co-located.
!        Substitute these x',y' expressions into d2 and expand.  The
!        basic product terms are pre-computed for speed.
!        Note that X and Y here are written in the conventional lon and
!        lat definitions, not the PPS transpose, but this doesn't matter.
!
!test
!        WRITE (*,*) 'fx0,fy0,nx0,ny0,nxmx,nymx,nxa,nxb,nya,nyb,xor,yor',
!     +                    fx0,fy0,nx0,ny0,nxmx,nymx,nxa,nxb,nya,nyb,xor,yor
!        WRITE (*, *) 'x,y,d2,i_d2'
!test
        DO  j = nya, nyb
        DO  i = nxa, nxb
            IF  ( i .LT. 1 )  THEN
                ii = i + LN
              ELSE IF  ( i .GT. LN )  THEN
                ii = i - LN
              ELSE
                ii = i
            END IF
            ipps = ii
            jpps = LTS + 1 - j
            x = ( ( FLOAT (i) - 0.5 ) - fx0 ) * cosl
            y =   ( FLOAT (j) - 0.5 ) - fy0
            xx =  x * x
            xy =  x * -y
            yy = -y * -y
            d2 = ( xx * xor2 + xy * xoryor2 + yy * yor2 ) * hor2s2 + &
                 ( xx * yor2 - xy * xoryor2 + yy * xor2 ) * hor2w2
            i_d2 = AMIN1 ( 50., 10. * d2 )
            IF  ( i_d2 .LT. influ (ii, j) )  THEN
!
!                Note that influ is in [i,j] coordinates.
!
                influ   (ii, j)         = i_d2
                gprtemp (jpps, ipps, 1) = fp
            !vvvvvv YDT 20240428
                !YDT gprtemp (jpps, ipps, 2) = 1
                !write(*, *) "jpps=", jpps, " ipps=", ipps, " fp=", fp
                if (gprtemp (jpps, ipps, 2) .eq. -9999.0 ) then
                gprtemp (jpps, ipps, 2)= 1
            else 
                gprtemp (jpps, ipps, 2)= gprtemp (jpps, ipps, 2)+1
            end if 
            !^^^^^^ YDT 20240428

                IF  ( NINT(100.*fp) .GT. 0 )  THEN
                    gprtemp (jpps, ipps, 3) =  1.
                  ELSE
                    gprtemp (jpps, ipps, 3) =  0.
                END IF
                gprtemp (jpps, ipps, 4) = fa
            END IF
!test
!            WRITE (*, *) x,y,d2,i_d2
!test
        END DO
        END DO
!
        RETURN
        END
