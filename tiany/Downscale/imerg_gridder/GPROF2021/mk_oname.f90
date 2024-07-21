	SUBROUTINE MK_ONAME  ( outpath_g, algid, satid, instrument, &
			       istime, ietime, oname )
!************************************************************************
!* MK_ONAME  Create a local output filename for a GPROF swath NN grid	*
!*									*
!* Called by:	GPROF2021OUT						*
!*									*
!* Calls:	<none>							*
!*									*
!* This subroutine uses the start and stop times to build a level 3	*
!* file name of the form <outpath_g>/					*
!*   <alg_id>.<satellite>.<instrument>.<start_date>_<start_time>_	*
!*		   <end_date>_<end_time>				*
!* where								*
!*   <outpath_g>    = directory for gridded output, with trailing slash *
!*   <alg_id>	    = algorithm id (e.g. "GPROF2021_NNG", where NNG =	*
!*				    'nearest-neighbor grid')		*
!*   <satellite>    = name of satellite (e.g., "GPM", "NOAA19")		*
!*   <instrument>   = name of instrument (e.g., "GMI", "MHS")		*
!*   <start_date>   = YYYYMMDD						*
!*   <start_time>   = HHMMSS						*
!*   <end_date>     = YYYYMMDD						*
!*   <end_time>     = HHMMSS						*
!*									*
!* Input parameters:							*
!*   outpath_g  CHAR	output directory, including trailing slash (/)  *
!*   algid	CHAR	algorithm id (e.g., GPROF2021_NNG)		*
!*   satid	CHAR	name of satellite (e.g., GPM, NOAA19)		*
!*   instrument	CHAR	name of instrument (e.g., GMI, MHS)		*
!*   istime	INT4*6	Date/time of orbit start			*
!*   ietime	INT4*6	Date/time of orbit stop				*
!*									*
!* Output parameters:							*
!*   oname	CHAR*	L.3 2AGPROF2021 grid file name			*
!**									*
!* Log:									*
!* E.Nelkin/SSAI	08/13	Adapt GPROF2010V2/mk_oname.f90		*
!* E.Nelkin/SSAI	12/13	Generic version: incorporate instrument *
!* E.Nelkin/SSAI	01/14	Incorporate algid, satid in oname	*
!* E.Nelkin/SSAI	02/15	Expand outpath_g, oname lengths to 1024 *
!* E.Nelkin/SSAI        05/21   GPROF 2020 version                      *
!* E.Nelkin/SSAI        01/22   Update GPROF2020 names to GPROF2021     *
!************************************************************************
	IMPLICIT	NONE
	INTEGER*4	:: j
	CHARACTER*4	:: cyyyys, cyyyye
	CHARACTER*2	:: cmms, cdds, chhs, cmns, csss
	CHARACTER*2	:: cmme, cdde, chhe, cmne, csse
!
	CHARACTER*256	:: algid, satid, instrument
	CHARACTER*1024	:: outpath_g, oname
	INTEGER*4	:: istime (6), ietime (6)
!       
!	Iniitalize character strings to blanks.
!
	do j = 1, 4
	   cyyyys(j:j) = ' '
	   cyyyye(j:j) = ' '
	end do
	do j = 1, 2
	   cmms(j:j) = ' '
	end do
	cdds = cmms
	chhs = cmms
	cmns = cmms
	csss = cmms
	cmme = cmms
	cdde = cmms
	chhe = cmms
	cmne = cmms
	csse = cmms
!
!	Convert start date/time to characters.
!
	WRITE (cyyyys,  10) istime (1)
   10	FORMAT ( I4.4 )
	WRITE (cmms,    11) istime (2)
   11	FORMAT ( I2.2 )
	WRITE (cdds,    11) istime (3)
	WRITE (chhs,    11) istime (4)
	WRITE (cmns,    11) istime (5)
	WRITE (csss,    11) istime (6)
!
!	Convert end date/time to characters.
!
	WRITE (cyyyye,  10) ietime (1)
	WRITE (cmme,    11) ietime (2)
	WRITE (cdde,    11) ietime (3)
	WRITE (chhe,    11) ietime (4)
	WRITE (cmne,    11) ietime (5)
	WRITE (csse,    11) ietime (6)
!
!	Build the output file name.
!
	oname = TRIM(outpath_g) // TRIM(algid) // '.' // &
		TRIM(satid) // '.' // TRIM(instrument) // &
		'.' // cyyyys //  cmms // cdds // '_' // chhs // &
		cmns // csss // '_' // cyyyye // cmme // cdde // &
		'_' // chhe // cmne // csse
!
	RETURN
	END
