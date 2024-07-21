!***********************************************************************
!	PARM_RES.H						       *
!								       *
!	Parameter header file specifying the grid size resolution of   *
!	the input orbits for GPROF2014.				       *
!								       *
!	LN - number of global longitude grid boxes		       *
!	LT - number of global latitude grid boxes		       *
!	ILATEXT - starting latitude of data extent in degrees	       *
!		  (assume symmetrical)				       *
!	LTS - number of central CED latitude grid boxes		       *
!	LT10 - number of latitude grid boxes for the 1.0-degree        *
!	       histogram, treated as the latitude extent * 2	       *
!	NNSFRAC - NS grid size as gridboxes per degree		       *
!	NCEDFRAC - NS grid size as gridboxes per degree		       *
!	NNS2CED - NS grid increments in a CED grid increment	       *
!	NNSX - X size of north and south PTP			       *
!	NNSY - Y size of north and south PTP			       *
!								       *
!**								       *
!* Log:								       *
!* G.Huffman/612	05/16	re-introduce Polar Tangent Plane scheme*
!*				for fully global coverage, requiring   *
!*				NNSX, NNSY			       *
!***********************************************************************
!
	INTEGER*4	:: LN, LT, LTS, ILATEXT, LT10, NNSFRAC, &
      			 NNS2CED, NCEDFRAC, NNSX, NNSY
	PARAMETER	(LN       = 3600, &
      			 LT       = 1800, &
      			 ILATEXT  = 75, &
      			 LTS      = ILATEXT * (LN / 360) * 2, &
			 LT10     = ILATEXT * 2, &
      			 NNSFRAC  = 40, &
      			 NCEDFRAC = 10, &
      			 NNS2CED  = NNSFRAC / NCEDFRAC, &
			 NNSX     = ( 90 - ILATEXT ) * NNSFRAC * 2, &
      			 NNSY     = ( 90 - ILATEXT ) * NNSFRAC * 2)


