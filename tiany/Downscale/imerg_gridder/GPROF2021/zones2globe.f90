	SUBROUTINE ZONES2GLOBE (LN, LT, LTS, NNS2CED, NNSFRAC, &
			   NCEDFRAC, NNSX, NNSY, gprtemp4c, &
			   gprtimec, gprtemp4n, gprtimen, gprtemp4s, &
			   gprtimes, gprglobe, iret)
!
!***********************************************************************
!* ZONES2GLOBE	Combines north, central, south bands onto a global CED *
!*								       *
!* Called by:	GETGPROFG					       *
!*   								       *
!* Calls:							       *
!*  								       *
!* ZONES2GLOBE shifts the central Cylindrical Equi-Distant (CED) band  *
!* to the global CED grid, then remaps the north and south Polar       *
!* Tangent Planes (PTP) to the global CED grid (using nearest	       *
!* neighbor).							       *
!*  								       *
!* Input Parameters:						       *
!*								       *
!*  LN		INT	number of longitudinal grid boxes	       *
!*  LT		INT	number of latitudinal grid boxes	       *
!*  LTS		INT	number of latitudinal grid boxes in central CED*
!*  NNS2CED	INT	NS grid increments in a CED grid increment     *
!*  NNSFRAC	INT	NS grid size as gridboxes per degree	       *
!*  NCEDFRAC	INT	central grid size as gridboxes per degree      *
!*  NNSX	INT	X size of north and south PTP		       *
!*  NNSY	INT	Y size of north and south PTP		       *
!*  gprtemp4c	REAL	array of GPROF data mapped to central CED      *
!*  gprtimec	REAL	time (minutes after scan start on central CED) *
!*  gprtemp4n	REAL	array of GPROF data mapped to north PTP        *
!*  gprtimen	REAL	time (minutes after scan start on north PTP)   *
!*  gprtemp4s	REAL	array of GPROF data mapped to south PTP        *
!*  gprtimes	REAL	time (minutes after scan start on south PTP)   *
!* 								       *
!* Output Parameters:						       *
!*								       *
!*  gprglobe	REAL	Work array holding the global CED variables    *
!*  iret	INT	Return code: 0 = normal			       *
!* 								       *
!**								       *
!* Log:								       *
!* G.Huffman/612	05/16	Pull central from GPROF2014OUT, north  *
!*				and south from AMSU_GRID/FG_AVG	       *
!***********************************************************************
!
	IMPLICIT	NONE
!
#include "instruments.h"
!
	INTEGER*4	:: LN, LT, LTS, NNS2CED, NNSFRAC, NCEDFRAC, &
			   NNSX, NNSY
!
	INTEGER*4	:: i, j, k, iret
!
	REAL*4		:: FILL = -99.0
	REAL*4		:: gprtemp4c(LTS,LN,4), gprtimec(LTS,LN)
	REAL*4		:: gprtemp4n(NNSX,NNSY,4), gprtimen(NNSX,NNSY),&
			   gprtemp4s(NNSX,NNSY,4), gprtimes(NNSX,NNSY)
	REAL*4		:: gprglobe(LT,LN,5)
!
	REAL*4		:: cedrad, cedthet
	INTEGER*4	:: nfgx, nfgy, iflip, jtrans
!
!***********************************************************************
!	Fill the global CED array with missing values.
!
	DO  k = 1, 5
	DO  j = 1, LN
	DO  i = 1, LT
	    gprglobe(i,j,k) = FILL
	END DO
	END DO
	END DO
!
!***********************************************************************
!	Move the data from the central CED arrays to the global array.
!
	DO  k = 1, 4
	DO  j = 1, LN
	DO  i = 1, LTS
	    gprglobe(i + ((LT - LTS) / 2),j,k) = gprtemp4c(i,j,k)
	END DO
	END DO
	END DO
	DO  j = 1, LN
	DO  i = 1, LTS
	    gprglobe(i + ((LT - LTS) / 2),j,5) = gprtimec(i,j)
	END DO
	END DO
!
!***********************************************************************
!	* Note: gprtemp4n and gprtemp4s are (X,Y), but gprglobe is in
!	  PPS orientation - (lat,lon) as (S-->N,-180-->180).  Thus,
!	  the (i,j) location for storage is at (iflip,jtrans) - flipped
!	  N-to-S and translated 180 deg.
!	* In the north zone find the FG box closest to a CED box
!	  center.
!	* The loops are over the CED grid boxes covered by the
!	  north Tangent Polar grid.
!	* Converting lat/lon. to polar coordinates centered on the
!	  North Pole, cedrad is co-lat. in the PTP 0.025-deg-of-lat
!	  increments and cedthet is lon. in degrees.
!	* The PTP grid is a y-increasing-down left-handed coordinate,
!	  so the conversion from polar coordinates enters nfgy with a
!	  minus sign.
!       * Wrapping across lateral boundaries is not a concern in the
!         polar zones.
!	* nfgx and nfgy are not protected against out-of-bounds indices;
!	  it should never happen.
!
	DO  j = 1, LN
	    IF  ( j .GT. LN/2 )  THEN
	        jtrans = j - LN/2
	      ELSE
	        jtrans = j + LN/2
	    END IF
	    cedthet = ( j - 0.5 ) / NCEDFRAC
	    DO  i = 1, ((LT - LTS) / 2)
		iflip   = LT + 1 - i
		cedrad  = ( i - 0.5 ) * NNS2CED
		nfgx    = NINT (  cedrad * COSD ( cedthet ) + NNSX*0.5 )
		nfgy    = NINT ( -cedrad * SIND ( cedthet ) + NNSY*0.5 )
!!
!!		IF  ( nfgx .LT. 1 )  THEN
!!		    WRITE (*, *) 'i,j,cedrad,cedthet,nfgx=',i,j,cedrad,&
!!		    		 cedthet,nfgx
!!		    nfgx = 1
!!		  ELSE IF  ( nfgx .GT. NNSX )  THEN
!!		    WRITE (*, *) 'i,j,cedrad,cedthet,nfgx=',i,j,cedrad,&
!!		    		 cedthet,nfgx
!!		    nfgx = NNSX
!!		END IF
!!		IF  ( nfgy .LT. 1 )  THEN
!!		    WRITE (*, *) 'i,j,cedrad,cedthet,nfgy=',i,j,cedrad,&
!!		    		 cedthet,nfgy
!!		    nfgx = 1
!!		  ELSE IF  ( nfgy .GT. NNSY )  THEN
!!		    WRITE (*, *) 'i,j,cedrad,cedthet,nfgy=',i,j,cedrad,&
!!		    		 cedthet,nfgy
!!		    nfgx = NNSY
!!		END IF
!!
		DO  k = 1, 4
		    gprglobe(iflip,jtrans,k) = gprtemp4n(nfgx,nfgy,k)
		END DO
		gprglobe(iflip,jtrans,5) = gprtimen(nfgx,nfgy)
	    END DO
	END DO
!
!	Now repeat for the south zone.  The code is identical to the
!	north zone except for:
!	* i ranging over the southern zone,
!	* i and cedrad incrementing in opposite directions,
!	* j and cedthet incrementing in opposite directions, and
!	* reference to gprtemp4s and gprtimes,
!
	DO  j = 1, LN
	    IF  ( j .GT. LN/2 )  THEN
	        jtrans = j - LN/2
	      ELSE
	        jtrans = j + LN/2
	    END IF
	    cedthet = ( LN - j + 0.5 ) / NCEDFRAC
	    DO  i = LT - ((LT - LTS) / 2) + 1, LT
		iflip   = LT + 1 - i
		cedrad  = ( LT - i + 0.5 ) * NNS2CED
		nfgx    = NINT (  cedrad * COSD ( cedthet ) + NNSX*0.5 )
		nfgy    = NINT ( -cedrad * SIND ( cedthet ) + NNSY*0.5 )
		DO  k = 1, 4
		    gprglobe(iflip,jtrans,k) = gprtemp4s(nfgx,nfgy,k)
		END DO
		gprglobe(iflip,jtrans,5) = gprtimes(nfgx,nfgy)
	    END DO
	END DO
!
	RETURN
	END
