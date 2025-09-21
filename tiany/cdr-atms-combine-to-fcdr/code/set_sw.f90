! set_sw_mod.f90
module set_sw_mod
  !YDT does not work until gfortran 15
  ! use iso_fortran_env, only: uint8
  use netcdf                      ! netCDF-Fortran module
  use constants
  use eswath_mod
  implicit none
  private
  public :: set_sw, check

contains

  pure function minmax2(a, numscan) result(mm)
    ! Compute MIN/MAX over first numscan rows, all NUMSPOT_A columns,
    ! ignoring values equal to MISSING.
    real, intent(in)           :: a(MAXSCANLINE_A, NUMSPOT_A)
    integer, intent(in)        :: numscan
    real                       :: mm(2)
    integer :: i, j
    real    :: amin, amax, v
    logical :: has

    amin = huge(1.0); amax = -huge(1.0); has = .false.
    do i = 1, numscan
      do j = 1, NUMSPOT_A
        v = a(i,j)
        if (v /= MISSING) then
          if (v < amin) amin = v
          if (v > amax) amax = v
          has = .true.
        end if
      end do
    end do
    if (has) then
      mm(1) = amin; mm(2) = amax
    else
      mm    = (/ MISSING, MISSING /)
    end if
  end function minmax2

  subroutine check(status, where)
    integer, intent(in) :: status
    character(*), intent(in), optional :: where
    
    if (status /= NF90_NOERR) then
      if ( present(where)) then
        write(*,*) 'Error in ', trim(where), ': ', trim(nf90_strerror(status))
      else 
        write(*,*) 'Error in ', trim(where), ': ', trim(nf90_strerror(status))
      end if
      stop 2
    end if
  end subroutine check

  subroutine set_sw(nc_filename, numscan)
    character(*), intent(in) :: nc_filename
    integer,      intent(in) :: numscan

    ! netCDF IDs
    integer :: ncid, grpid_geo, grpid_data
    integer :: x_dimid, y_dimid, c_dimid, ch_dimid
    integer :: varid, status
    integer :: dimids(2), cdimids(2), chandimids(2)
    integer :: chunks(2)
    integer :: chsize

    ! locals
    integer :: i, j, k
    real    :: fv
    real(8) :: fv_zero
    integer(1) :: fv_ubyte
    integer :: do_shuffle, do_deflate
    integer :: deflate_level

    ! strings
    character(len=32) :: conv, stdname_voc
    character(len=80) :: date_created
    character(len=:), allocatable :: summary, keywords, keywords_voc, title, prodver
    character(len=:), allocatable :: cdr_prog, cdr_vars, license, contrib_name, contrib_role
    character(len=:), allocatable :: creator_name, creator_url, creator_email, institution
    character(len=:), allocatable :: platform_local, sensor, cdm_type, naming_auth, metadata_link
    character(len=:), allocatable :: src, refs, hist, units_lon, units_lat, spatial_res
    character(len= NCHAR), dimension(:), allocatable :: scans  ! convenience alias

    ! buffers
    !YDT real                    :: data_out(MAXSCANLINE_A, NUMSPOT_A)
    real                    :: data_out(NUMSPOT_A, MAXSCANLINE_A) 
    real                    :: at_tmp
    character(len=1), allocatable :: scan_time_buf(:,:) ! (nscan, NCHAR)

    real :: lat_lim(2), lon_lim(2)
    integer(1) :: flag_orb(2)
    character(len=*), parameter :: str_orb   = 'northbound southbound'
    integer(1) :: flag_stype(3)
    character(len=*), parameter :: str_stype = 'ocean land coast'

    real :: latmm(2), lonmm(2)
    real(8) :: dtime

    ! channel name arrays (used only as attributes)
    character(len=*), parameter :: BT_Vname(NUMCHAN_A) = [ &
      character(len=32) :: &
      'fcdr_brightness_temperature_23', &
      'fcdr_brightness_temperature_31', &
      'fcdr_brightness_temperature_50', &
      'fcdr_brightness_temperature_89' ]

    character(len=*), parameter :: BT_Lname(NUMCHAN_A) = [ &
      character(len=64) :: &
      'NOAA FCDR of 23.8 GHz brightness temperature', &
      'NOAA FCDR of 31.4 GHz brightness temperature', &
      'NOAA FCDR of 50.3 GHz brightness temperature', &
      'NOAA FCDR of 89.0 GHz brightness temperature' ]

    character(len=*), parameter :: COORD(NUMCHAN_A) = [ &
      character(len=32) :: 'latitude_a2 longitude_a2', &
                           'latitude_a2 longitude_a2', &
                           'latitude_a1_2 longitude_a1_2', &
                           'latitude_a1_1 longitude_a1_1' ]

    !------------------------------------------------------------------
    ! Constants & options
    !------------------------------------------------------------------
    fv           = MISSING
    fv_zero      = 0.0_8
    fv_ubyte     = int(z'FF', kind=1)
    do_shuffle   = 1 
    do_deflate   = 1
    deflate_level= 1
    chsize       = MAXSCANLINE_A/5
    !YDT chunks       = (/ MAXSCANLINE_A/5, max(1, NUMSPOT_A/5) /)
    chunks       = (/  max(1, NUMSPOT_A/5), MAXSCANLINE_A/5 /)
    lat_lim      = (/ -90.0,  90.0 /)
    lon_lim      = (/ -180.0, 180.0 /)
    flag_orb     = (/ 0_1, 1_1 /)
    flag_stype   = (/ 0_1, 1_1, 2_1 /)

    conv         = 'CF-1.6'
    stdname_voc  = 'CF Standard Name Table (v34, 2016-06-13)'

    ! A few long text attributes
    title        = 'CICS Version-1 AMSUA FCDR'
    prodver      = 'V01R01-preliminary'
    summary      = 'CDRs for AMSU window channels and corresponding hydrological products '&
                   //'are vital for the climate community. The corrections applied to the '&
                   //'CDRs include geolocation correction, AMSU-A scan bias corrections, '&
                   //'and intersatellite calibration. The monthly initially submitted ' &
                   //'"preliminary" CDR data for the same time period will be replaced by '&
                   //'the "final" CDR data on an annual basis.'
    keywords     = 'EARTH SCIENCE > SPECTRAL/ENGINEERING > MICROWAVE > BRIGHTNESS TEMPERATURE'
    keywords_voc = 'NASA GCMD Earth Science Keywords (Version 8.1)'
    naming_auth  = 'gov.noaa.ncdc'
    metadata_link= 'gov.noaa.ncdc:C00980'
    sensor       = 'AMSU-A > Advanced Microwave Sounding Unit - A'
    cdm_type     = 'Swath'
    cdr_prog     = 'NOAA Climate Data Record Program for satellites, FY 2016'
    cdr_vars     = 'fcdr_brightness_temperature_23, fcdr_brightness_temperature_31, ' // &
                   'fcdr_brightness_temperature_50, fcdr_brightness_temperature_89'
    src          = adjustl(hdf_fname)   ! source HDF/Level1b file name
    refs         = 'Original level 1b data are from NOAA archive; also available at NOAA CLASS'
    hist         = 'TBD'
    license      = 'No restrictions on access or use'
    contrib_name = 'TBD'
    contrib_role = 'TBD'
    creator_name = 'Huan Meng, Ralph R Ferraro'
    creator_url  = 'http://cics.umd.edu/AMSU-CDR/home.html'
    creator_email= 'Huan.Meng@noaa.gov, Ralph.R.Ferraro@noaa.gov'
    institution  = 'DOC/NOAA/NESDIS/STAR/CoRP > Cooperative Research Program, ' // &
                   'Center for Satellite Applications and Research, NESDIS, NOAA, U.S. Department of Commerce'
    platform_local = adjustl(platform)
    units_lat    = 'degrees_north'
    units_lon    = 'degrees_east'
    spatial_res  = '48km X 48km at nadir, 150km X 80km at limb'

    ! ISO 8601 creation time in UTC
    call build_iso8601_utc(date_created)

    ! convenience alias
    !YDT scans => str_scantime
    allocate(scans(numscan)) 
    do i = 1, numscan
        scans(i) = str_scantime(i)
    end do 
      

    !------------------------------------------------------------------
    ! Create NetCDF-4 file
    !------------------------------------------------------------------
    status = nf90_create(trim(nc_filename), IOR(NF90_CLOBBER, NF90_NETCDF4), ncid); call check(status, 'nf90_create')

    ! Dimensions
    status = nf90_def_dim(ncid, 'nscan', numscan, x_dimid);   call check(status, 'def_dim nscan')
    status = nf90_def_dim(ncid, 'npixel', NUMSPOT_A, y_dimid);call check(status, 'def_dim npixel')
    status = nf90_def_dim(ncid, 'nchar', NCHAR, c_dimid);     call check(status, 'def_dim nchar')
    status = nf90_def_dim(ncid, 'nchan', NUMCHAN_A, ch_dimid);call check(status, 'def_dim nchan')

    ! -- seems need to swap the order 
    !dimids      = (/ x_dimid, y_dimid /)
    !cdimids     = (/ x_dimid, c_dimid /)     ! scan_time dims (nscan, nchar) as in C
    !chandimids  = (/ x_dimid, ch_dimid /)
    dimids      = (/ y_dimid, x_dimid /)
    cdimids     = (/ c_dimid, x_dimid /)     ! scan_time dims (nscan, nchar) as in C
    chandimids  = (/ ch_dimid, x_dimid /)

    write(*, *) "cdimids 0: ", cdimids
    !------------------------------------------------------------------
    ! Global attributes (mirror C)
    !------------------------------------------------------------------
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', "CF-1.6"))
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'Metadata_Conventions',  &
         'CF-1.6, Unidata Dataset Discovery v1.0, NOAA CDR v1.0, GDS v2.0')) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'standard_name_vocabulary', stdname_voc)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'id',                    trim(nc_fname)) )
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'naming_authority',      naming_auth))
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'metadata_link',         metadata_link)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'title',                 title)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'product_version',       prodver)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'date_issued',           'TBD')) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'summary',               summary)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'keywords',              keywords)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'keywords_vocabulary',   keywords_voc)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'platform',              platform_local)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'sensor',                sensor)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'cdm_data_type',         cdm_type)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'cdr_program',           cdr_prog)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'cdr_variable',          cdr_vars)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'source',                src)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'references',            refs)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'history',               hist)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'date_created',          date_created)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'creator_name',          creator_name)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'creator_url',           creator_url)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'creator_email',         creator_email)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'institution',           institution))
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'processing_level',      'NOAA level 2')) 

    write(*, *) "cdimids 0.1: ", cdimids
    ! Geo coverage from a1_1 (as in C)
    latmm = minmax2(lat_a1_1, numscan)
    lonmm = minmax2(lon_a1_1, numscan)
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'geospatial_lat_min', latmm(1))) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'geospatial_lat_max', latmm(2))) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'geospatial_lon_min', lonmm(1))) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'geospatial_lon_max', lonmm(2))) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'geospatial_lat_units', units_lat)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'geospatial_lon_units', units_lon)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'spatial_resolution', spatial_res)) 

    ! time_coverage_* (strings from str_scantime and diff of time_tai93)
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'time_coverage_start', trim(scans(1)))) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'time_coverage_end',   trim(scans(numscan)))) 
    dtime = time_tai93(numscan) - time_tai93(1)
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'time_coverage_duration', 'P'//trim(to_int_string(nint(dtime)))//'S')) 

    call check(nf90_put_att(ncid, NF90_GLOBAL, 'license',          license)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'contributor_name', contrib_name)) 
    call check(nf90_put_att(ncid, NF90_GLOBAL, 'contributor_role', contrib_role)) 

    !------------------------------------------------------------------
    ! Group: Geolocation_Time_Fields
    !------------------------------------------------------------------
    call check(nf90_def_grp(ncid, 'Geolocation_Time_Fields', grpid_geo)) 

    ! scan_time (char[nscan, nchar]) – keep same order as C
    call check(nf90_def_var(grpid_geo, 'scan_time', NF90_CHAR, cdimids, varid)) 
    call check(nf90_put_att(grpid_geo, varid, 'long_name', &
          'Scan start time (UTC) in ISO8601 date/time (YYYY-MM-DDTHH-MM-SS) format')) 
    call check(nf90_put_att(grpid_geo, varid, '_FillValue', '0')) 

    if (numscan > chsize) then
      call check(nf90_def_var_chunking(grpid_geo, varid, NF90_CHUNKED, chunks), 'chuncking here') 
      call check(nf90_def_var_deflate (grpid_geo, varid, do_shuffle, do_deflate, deflate_level), 'deflate here') 
    end if

    write(*, *) "cdimids for scan_time: ", cdimids
    !YDT 9/21/2025: the following !-- lines are not needed. 
    ! Build a 2-D char buffer [nscan, nchar] from string array
    !YDT allocate(scan_time_buf(NCHAR, numscan))
    !-- allocate(scan_time_buf(numscan, NCHAR))
    !-- do i = 1, numscan
    !--   do j = 1, NCHAR
    !--     !YDT scan_time_buf(j, i) = scans(i)(j:j)
    !--     scan_time_buf(i, j) = scans(i)(j:j)
    !--   end do
    !-- end do
    !--  
    !-- call check(nf90_put_var(grpid_geo, varid, scan_time_buf), 'put_var scan_time')

    call check(nf90_put_var(grpid_geo, varid, scans), 'put_var scan_time')
    !-- deallocate(scan_time_buf)

    ! scan_time_since98 (double seconds since 1998-01-01T00:00:00)
    call check(nf90_def_var(grpid_geo, 'scan_time_since98', NF90_DOUBLE, x_dimid, varid)) 
    call check(nf90_put_att(grpid_geo, varid, 'standard_name', 'time')) 
    call check(nf90_put_att(grpid_geo, varid, 'long_name', &
        'Scan start time (UTC) in a referenced or elapsed time format')) 
    call check(nf90_put_att(grpid_geo, varid, '_FillValue', fv_zero), 'fill vz_zero') 
    call check(nf90_put_att(grpid_geo, varid, 'units', & 
         'seconds since 1998-01-01T00:00:00')) 
    if (numscan > chsize) then
      call check(nf90_def_var_chunking(grpid_geo, varid, NF90_CHUNKED, chunks(1:1))) 
      call check(nf90_def_var_deflate (grpid_geo, varid, do_shuffle, do_deflate, deflate_level)) 
    end if
    call check(nf90_put_var(grpid_geo, varid, time_tai93(1:numscan))) 

    write(*, *) "here 1: "

    ! Latitude/Longitude fields (a1_1, a1_2, a2)
    call def_put_geo(grpid_geo, 'latitude_a1_1',  NF90_FLOAT, dimids, lat_lim, units_lat, lat_a1_1, numscan)
    call def_put_geo(grpid_geo, 'latitude_a1_2',  NF90_FLOAT, dimids, lat_lim, units_lat, lat_a1_2, numscan)
    call def_put_geo(grpid_geo, 'latitude_a2',    NF90_FLOAT, dimids, lat_lim, units_lat, lat_a2,    numscan)

    call def_put_geo(grpid_geo, 'longitude_a1_1', NF90_FLOAT, dimids, lon_lim, units_lon, lon_a1_1, numscan)
    call def_put_geo(grpid_geo, 'longitude_a1_2', NF90_FLOAT, dimids, lon_lim, units_lon, lon_a1_2, numscan)
    call def_put_geo(grpid_geo, 'longitude_a2',   NF90_FLOAT, dimids, lon_lim, units_lon, lon_a2,   numscan)

    write(*, *) "here 1.2: "
    !------------------------------------------------------------------
    ! Group: Data_Fields
    !------------------------------------------------------------------
    call check(nf90_def_grp(ncid, 'Data_Fields', grpid_data)) 

    ! orbital_mode (UBYTE)
    call check(nf90_def_var(grpid_data, 'orbital_mode', NF90_UBYTE, x_dimid, varid)) 
    call check(nf90_put_att(grpid_data, varid, 'long_name', 'satellite direction')) 
    call check(nf90_put_att(grpid_data, varid, 'flag_values', flag_orb)) 
    call check(nf90_put_att(grpid_data, varid, 'flag_meanings', str_orb)) 
    !YDT  no idea how to make f90 produce UBYTE data
    !call check(nf90_put_att(grpid_data, varid, '_FillValue', fv_ubyte), 'fill fv_ubyte') 
    if (numscan > chsize) then
      !YDT call check(nf90_def_var_chunking(grpid_data, varid, NF90_CHUNKED, chunks(1:1))) 
      call check(nf90_def_var_chunking(grpid_data, varid, NF90_CHUNKED, chunks(2:2))) 
      call check(nf90_def_var_deflate (grpid_data, varid, do_shuffle, do_deflate, deflate_level)) 
    end if
    write(*, *) "here 1.5: "
    ! NOTE: In C orb_mode[] was char. Ensure you provide integer(1) values 0/1
    !       in some module variable if you need this. If not available, comment next line.
    ! call check(nf90_put_var(grpid_data, varid, orb_mode(1:numscan)), 'put_var orbital_mode')

    ! surface_type_*  (BYTE)
    call def_put_stype(grpid_data, 'surface_type_a1_1', dimids, stype_a1_1, numscan, &
          'surface type for AMSU unit A1-1, e.g., 89.0 GHz', flag_stype, str_stype)
    call def_put_stype(grpid_data, 'surface_type_a1_2', dimids, stype_a1_2, numscan, & 
          'surface type for AMSU unit A1-2, e.g., 50.3 GHz', flag_stype, str_stype)
    call def_put_stype(grpid_data, 'surface_type_a2',   dimids, stype_a2,   numscan, & 
          'surface type for AMSU unit A2, including 23.8 GHz and 31.4 GHz', flag_stype, str_stype)

    write(*, *) "here 1.6: "
    ! Earth incidence and solar zenith angles
    call def_put_angle(grpid_data, 'earth_angle_of_incidence_a1_1', & 
          dimids, lza_a1_1, numscan, COORD(4))
    call def_put_angle(grpid_data, 'earth_angle_of_incidence_a1_2', & 
          dimids, lza_a1_2, numscan, COORD(3))
    call def_put_angle(grpid_data, 'earth_angle_of_incidence_a2',   & 
          dimids, lza_a2,   numscan, COORD(1))
    call def_put_angle(grpid_data, 'solar_zenith_angle',            & 
          dimids, sza,      numscan, '')

    write(*, *) "here 2: "
    ! Brightness temperature (apply asym correction, round to 0.01 K)
    do k = 1, NUMCHAN_A
      
      call check(nf90_def_var(grpid_data, BT_Vname(k), NF90_FLOAT, dimids, varid)) 
      call check(nf90_put_att(grpid_data, varid, 'standard_name', 'brightness_temperature')) 
      call check(nf90_put_att(grpid_data, varid, 'long_name', BT_Lname(k)), 'att BT long_name')
      call check(nf90_put_att(grpid_data, varid, 'valid_min', limit_A%Temp_lower(k))) 
      call check(nf90_put_att(grpid_data, varid, 'valid_max', limit_A%Temp_upper(k))) 
      call check(nf90_put_att(grpid_data, varid, '_FillValue', fv), 'fill fv') 
      call check(nf90_put_att(grpid_data, varid, 'units', 'kelvin')) 
      call check(nf90_put_att(grpid_data, varid, 'coordinates', COORD(k))) 

      if (numscan > chsize) then
        call check(nf90_def_var_chunking(grpid_data, varid, NF90_CHUNKED, chunks), 'chunk BT')
        call check(nf90_def_var_deflate (grpid_data, varid, do_shuffle, do_deflate, deflate_level)) 
      end if

      ! compute & round
      do i = 1, numscan
        do j = 1, NUMSPOT_A
          if (at(i,j,k) /= MISSING) then
            at_tmp = at(i,j,k) * (1.0 + asym_a1(j,k)) + asym_a0(j,k)
            at_tmp = at_tmp + asym_a2(j,k) * at(i,j,k) * at(i,j,k)
            !YDT data_out(i,j) = nint(at_tmp*100.0)/100.0
            data_out(j,i) = nint(at_tmp*100.0)/100.0
          else
            !YDT data_out(i,j) = fv
            data_out(j, i) = fv
            ! set bit 7 in qa_prod(i,k)
            !YDT qa_prod(i,k) = int( ibset(int(qa_prod(i,k),kind=4), 7), kind=1 )
            qa_prod(k, i) = int( ibset(int(qa_prod(k, i),kind=4), 7), kind=1 )
          end if
        end do
      end do

      call check(nf90_put_var(grpid_data, varid, data_out(1:NUMSPOT_A, 1:numscan))) 
      write(*, *) "channel k=", k, " done"
    end do

    ! Product Quality Flag (UBYTE) [nscan,nchan]
    call check(nf90_def_var(grpid_data, 'flag_brightness_temperature', NF90_UBYTE, chandimids, varid)) 
    call check(nf90_put_att(grpid_data, varid, 'long_name', 'instrument/channel quality flag')) 
    call check(nf90_put_att(grpid_data, varid, 'comment', &
      'The 8-bit quality flags are channel-specific. ' &
      // ' Users are advised not to use any scans for which' &
      // ' the highest bit (bit 7) is set' & 
      // ' to 1. The lowest two bits (bits 0 and 1) are not used.' &
      // ' The meanings of the bits from the MSB (bit 7) to the ' & 
      // ' LSB (bit 0), if set to 1, are: ' &
      // ' bit 7 = Do not use scan for product generation; ' & 
      // ' bit 6 = Calibration error; ' & 
      // ' bit 5 = Time sequence error; ' & 
      // ' bit 4 = Earth location questionable;' & 
      // ' bit 3 = Brightness temperature out of range;' & 
      // ' bit 2 = Lunar contamination warning; ' &
      // ' bit 1~0 = zero fill. ' & 
      // ' If any bit is set to 0, then there is no correspondent error or warning.' )) 
    ! Following your C: small "flag_masks" example; adjust if you have a full mask array
    call check(nf90_put_att(grpid_data, varid, 'flag_masks', flag_orb)) 
    call check(nf90_put_att(grpid_data, varid, 'flag_meanings', 'good problematic')) 

    call check(nf90_put_var(grpid_data, varid, qa_prod(1:NUMCHAN_A, 1:numscan))) 
    write(*, *) "Here 3:" 

    ! Close
    call check(nf90_close(ncid), 'close')
    write(*,'(A,1X,A)') '** SUCCESS writing example file', trim(nc_filename)
  end subroutine set_sw

  !----------------- helpers used above -----------------

  subroutine build_iso8601_utc(out)
    character(len=*), intent(out) :: out
    integer :: vals(8)
    call date_and_time(values=vals)
    write(out,'(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)') vals(1),vals(2),vals(3), vals(5),vals(6),vals(7)
  end subroutine build_iso8601_utc

  pure function to_int_string(i) result(s)
    integer, intent(in) :: i
    character(len=32) :: s
    write(s,'(I0)') i
  end function to_int_string

  subroutine def_put_geo(gid, name, xtype, dims, lim, units, arr, nscan)
    integer, intent(in) :: gid, xtype, dims(2), nscan
    character(*), intent(in) :: name, units
    real, intent(in) :: lim(2)
    real, intent(in) :: arr(MAXSCANLINE_A, NUMSPOT_A)
    integer :: vid, status
    integer :: chunks(2)

    !YDT chunks = (/ MAXSCANLINE_A/5, max(1, NUMSPOT_A/5) /)
    chunks = (/ max(1, NUMSPOT_A/5), MAXSCANLINE_A/5 /)

    call check(nf90_def_var(gid, name, xtype, dims, vid)) 
    call check(nf90_put_att(gid, vid, 'standard_name', trim(adjustl(name(1:8)) ) ) ) ! "latitude"/"longitude" in the names already
    call check(nf90_put_att(gid, vid, 'valid_min', lim(1))) 
    call check(nf90_put_att(gid, vid, 'valid_max', lim(2))) 
    call check(nf90_put_att(gid, vid, '_FillValue', real(MISSING) ), 'inside def_put_geo') 
    call check(nf90_put_att(gid, vid, 'units', units)) 

    if (nscan > MAXSCANLINE_A/5) then
      call check(nf90_def_var_chunking(gid, vid, NF90_CHUNKED, chunks)) 
      !call check(nf90_def_var_deflate (gid, vid, .true., .true., 1)) 
      call check(nf90_def_var_deflate (gid, vid, 1, 1, 1)) 
    end if

    !YDT call check(nf90_put_var(gid, vid, arr(1:nscan, 1:NUMSPOT_A))) 
    call check(nf90_put_var(gid, vid, arr(1:NUMSPOT_A, 1:nscan))) 
  end subroutine def_put_geo

  subroutine def_put_angle(gid, name, dims, arr, nscan, coord)
    integer, intent(in) :: gid, dims(2), nscan
    character(*), intent(in) :: name, coord
    !YDT real, intent(in) :: arr(MAXSCANLINE_A, NUMSPOT_A)
    real, intent(in) :: arr(NUMSPOT_A, MAXSCANLINE_A)
    integer :: vid, status
    integer :: chunks(2)

    !YDT chunks = (/ MAXSCANLINE_A/5, max(1, NUMSPOT_A/5) /)
    chunks = (/ max(1, NUMSPOT_A/5), MAXSCANLINE_A/5 /)

    call check(nf90_def_var(gid, name, NF90_FLOAT, dims, vid)) 
    call check(nf90_put_att(gid, vid, 'long_name', name//' for AMSU')) 
    call check(nf90_put_att(gid, vid, '_FillValue', real(MISSING) )) 
    call check(nf90_put_att(gid, vid, 'units', 'degree')) 
    if (len_trim(coord) > 0) call check(nf90_put_att(gid, vid, 'coordinates', coord) ) 

    if (nscan > MAXSCANLINE_A/5) then
      call check(nf90_def_var_chunking(gid, vid, NF90_CHUNKED, chunks)) 
      !call check(nf90_def_var_deflate (gid, vid, .true., .true., 1)) 
      call check(nf90_def_var_deflate (gid, vid, 1, 1, 1)) 
    end if

    !YDT call check(nf90_put_var(gid, vid, arr(1:nscan,1:NUMSPOT_A))) 
    call check(nf90_put_var(gid, vid, arr(1:NUMSPOT_A, 1:nscan))) 
  end subroutine def_put_angle

  subroutine def_put_stype(gid, name, dims, arr, nscan, long_name, flags, meanings)
    integer, intent(in) :: gid, dims(2), nscan
    character(*), intent(in) :: name, long_name, meanings
    !YDT integer(1), intent(in) :: arr(MAXSCANLINE_A, NUMSPOT_A)
    integer(1), intent(in) :: arr(NUMSPOT_A, MAXSCANLINE_A)
    integer(1), intent(in) :: flags(:)
    integer :: vid, status
    integer :: chunks(2)

    !YDT chunks = (/ MAXSCANLINE_A/5, max(1, NUMSPOT_A/5) /)
    chunks = (/ max(1, NUMSPOT_A/5), MAXSCANLINE_A/5 /)

    call check(nf90_def_var(gid, name, NF90_BYTE, dims, vid)) 
    call check(nf90_put_att(gid, vid, 'long_name', long_name)) 
    call check(nf90_put_att(gid, vid, 'flag_values', flags)) 
    call check(nf90_put_att(gid, vid, 'flag_meanings', meanings)) 

    if (nscan > MAXSCANLINE_A/5) then
      call check(nf90_def_var_chunking(gid, vid, NF90_CHUNKED, chunks)) 
      !call check(nf90_def_var_deflate (gid, vid, .true., .true., 1)) 
      call check(nf90_def_var_deflate (gid, vid, 1, 1, 1)) 
    end if

    !YDT call check(nf90_put_var(gid, vid, arr(1:nscan,1:NUMSPOT_A)) ) 
    call check(nf90_put_var(gid, vid, arr(1:NUMSPOT_A, 1:nscan)) ) 
  end subroutine def_put_stype

end module set_sw_mod

