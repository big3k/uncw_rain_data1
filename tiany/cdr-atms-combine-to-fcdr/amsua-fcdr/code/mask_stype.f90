module mask_stype_mod
    use, intrinsic :: iso_fortran_env, only : real32, int8
    use netcdf                      ! netCDF-Fortran module
    use constants
    use eswath_mod
    implicit none
    private


   integer, parameter :: dp  = real32
   integer, parameter :: i1  = int8      ! 1-byte integer (replacement for C 'char' used 

    public :: read_mask, read_latbox_table, mask_stype 

 

contains 

  subroutine read_mask() 
    implicit none
    character(len=*), parameter :: filename = "../input/mask.bin"
    integer :: unit, ios

    unit = 10  ! Arbitrary unit number

    open(unit=unit, file=filename, form='unformatted', access='stream', &
         status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'Error opening file:', filename
        stop
    end if

    read(unit) mask

    close(unit)

    print *, 'Successfully read mask data!'

   end subroutine read_mask

   subroutine read_latbox_table() 
    implicit none
    character(len=*), parameter :: filename = "../input/latbox_table96.dat"
    integer :: unit, i, intdum, ios 

    unit = 20  ! Arbitrary unit number

    open(unit=unit, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'Error opening file:', filename
        stop
    end if

    do i = 1, NUMSPOT_A
        read(unit, *, iostat=ios) latbox_up(i), latbox_down(i), intdum
        if (ios /= 0) then
            print *, 'Error reading line', i
            exit
        end if
    end do

    close(unit)

   end subroutine read_latbox_table

  !---------------------------------------------------------------------------
  ! Subroutine: mask_stype
  !
  ! Purpose:
  !   Generate per-FOV surface type (ocean=0, land=1, coast=2) by integrating
  !   an 8 km (1/16°) land/sea mask within each FOV footprint.
  !
  ! Interface mirrors C signature:
  !   C: char** mask_stype(TOTNUM_OF_SCAN numscan, float lat[][NUMSPOT_A], float lon[][NUMSPOT_A])
  !
  ! Fortran:
  !   Inputs:
  !     numscan          - number of valid scan lines
  !     lat(numscan,NUMSPOT_A)  [deg]
  !     lon(numscan,NUMSPOT_A)  [deg], -180..180 or 0..360 (see notes)
  !
  !   Output:
  !     stype(numscan,NUMSPOT_A): integer(i1)  (0=ocean, 1=land, 2=coast)
  !
  ! Notes on indexing:
  !   - We keep Fortran arrays 1-based for (scan,fov).
  !   - Internally, the mask index math follows the original C 0-based scheme,
  !     and we add +1 when addressing 'mask' (declared 1-based here).
  !   - The logic/thresholds (>=0.99 land, <=0.01 ocean) are identical.  
  !---------------------------------------------------------------------------
  subroutine mask_stype(numscan, lat, lon, stype)
    integer,               intent(in)  :: numscan
    real,              intent(in)  :: lat(:,:)      ! (NUMSPOT_A, numscan)
    real,              intent(in)  :: lon(:,:)      ! (NUMSPOT_A, numscan) 
    integer(1),           intent(out) :: stype(:,:)    ! (NUMSPOT_A, numscan) 

    ! locals
    integer :: iscan, ifov, i, j
    integer :: lat_pts, latbot_mask, lattop_mask
    integer :: lonleft_mask, lonright_mask
    integer :: indx
    integer :: halfspots
    real :: ang(0:NUMSPOT_A)
    real :: fovsize(NUMSPOT_A)
    real :: angle, angle1
    real :: fov_size, loncorr, sum
    real :: alat, alon
    real, parameter :: lon0 = 180.0
    real, parameter :: lat0 =  90.0

    !write(*, *)"min/max of lat, lon:", minval(lat), maxval(lat), minval(lon), maxval(lon) 

    ! --- Calculate size of each FOV (degrees) across the scan, preserving source math ---
    halfspots = NUMSPOT_A / 2      ! integer division like C
    do i = 0, NUMSPOT_A
       angle  = PI * SCAN_ANG_A * real(i - halfspots, dp) / 180.0_dp
       angle1 = (REARTH + RSAT) * sin(angle) / REARTH
       angle1 = atan( angle1 / sqrt( max(1.0_dp - angle1*angle1, 1.0e-12_dp) ) )
       ang(i) = (angle1 * 180.0_dp / PI) - SCAN_ANG_A * real(i - halfspots, dp)
       if (i > 0) fovsize(i) = abs( ang(i) - ang(i-1) )
       !YDT
       !write(*, *) i, angle, fovsize(i)
    end do

    ! --- Main loop over scans and FOVs ---
    do iscan = 1, numscan
       do ifov = 1, NUMSPOT_A
          fov_size = fovsize(ifov)
          alat     = lat(ifov, iscan) 
          alon     = lon(ifov, iscan) 
          !write(*, '(2F10.2)')  alon, alat !YDT

          ! Determine mask-coordinate bounds of the FOV footprint
          loncorr       = abs( 1.0_dp / cos(PI * alat / 180.0_dp) )
          lonleft_mask  = int( (alon + lon0 - fov_size * loncorr) * MASK_RESOLUTION )      ! trunc toward 0
          lonright_mask = int( (alon + lon0 + fov_size * loncorr) * MASK_RESOLUTION )

          if (lonleft_mask < 0)              lonleft_mask  = 0
          if (lonright_mask > MAP_COLS - 1)  lonright_mask = MAP_COLS - 1

          lat_pts     = MAP_ROWS - int( (alat + lat0) * MASK_RESOLUTION )
          latbot_mask = lat_pts - latbox_up(ifov)
          lattop_mask = lat_pts + latbox_down(ifov)

          if (lattop_mask > MAP_ROWS - 1) lattop_mask = MAP_ROWS - 1
          if (latbot_mask > MAP_ROWS - 1) latbot_mask = MAP_ROWS - 1
          if (lattop_mask < 0)            lattop_mask = 0
          if (latbot_mask < 0)            latbot_mask = 0

          ! Integrate mask values over the rectangular footprint
          sum  = 0.0_dp
          indx = 0

          if ( (lonleft_mask < 0) .and. (lonright_mask > MAP_COLS - 1) ) then
             ! Full wrap: use all columns
             do i = latbot_mask, lattop_mask
                do j = 0, MAP_COLS - 1
                   if (mask(j+1, i+1) == 1_i1) sum = sum + 1.0_dp
                   indx = indx + 1
                end do
             end do

          else if (lonleft_mask < 0) then
             do i = latbot_mask, lattop_mask
                do j = 0, lonright_mask
                   if (mask(j+1, i+1) == 1_i1) sum = sum + 1.0_dp
                   indx = indx + 1
                end do
                do j = MAP_COLS + lonleft_mask, MAP_COLS - 1
                   if (mask(j+1, i+1) == 1_i1) sum = sum + 1.0_dp
                   indx = indx + 1
                end do
             end do

          else if (lonright_mask > MAP_COLS - 1) then
             do i = latbot_mask, lattop_mask
                do j = lonleft_mask, MAP_COLS - 1
                   if (mask(j+1, i+1) == 1_i1) sum = sum + 1.0_dp
                   indx = indx + 1
                end do
                do j = 0, lonright_mask - MAP_COLS
                   if (mask(j+1, i+1) == 1_i1) sum = sum + 1.0_dp
                   indx = indx + 1
                end do
             end do

          else
             do i = latbot_mask, lattop_mask
                do j = lonleft_mask, lonright_mask
                   if (mask(j+1, i+1) == 1_i1) sum = sum + 1.0_dp
                   indx = indx + 1
                end do
             end do
          end if


          ! Classify: 0=ocean, 1=land, 2=coast  (same thresholds as C) 
          if (indx > 0) then
             sum = sum / real(indx, dp)
          else
             sum = 0.0_dp   ! degenerate footprint: treat as ocean
          end if


          if      (sum >= 0.99_dp) then
             stype(ifov, iscan) = 1_i1
          else if (sum <= 0.01_dp) then
             stype(ifov, iscan) = 0_i1
          else
             stype(ifov, iscan) = 2_i1
          end if

          !write(*, *) "footprint box: lat x lon, pix, fovsize, type: ", &
          !    alon, alat, &
          !    latbot_mask, lattop_mask, lonleft_mask, lonright_mask, indx, fov_size, stype(ifov, iscan)

       end do
    end do

  end subroutine mask_stype

end module mask_stype_mod


