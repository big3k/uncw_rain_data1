module mask_stype
    use netcdf                      ! netCDF-Fortran module
    use constants
    use eswath_mod
    implicit none
    private
    public :: read_mask, read_latbox_table 

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

end module mask_stype


