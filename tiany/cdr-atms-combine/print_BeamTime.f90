! 20250425: 
! Print the BeamTime variable in output nc file 
!  in the format of "YYYY-MM-DDTHH-MM-SS" 
! 
! /tmp/2012/20120303/NPP_ATMS_d20120303_t0210580_e0352176_b001798.nc

!ncdump -h /tmp/2012/20120303/NPP_ATMS_d20120303_t0210580_e0352176_b001798.nc -h
!netcdf NPP_ATMS_d20120303_t0210580_e0352176_b001798 {
!dimensions:
!              nchannel = 22 ;
!              nfovs = 96 ;
!              nscans = 2280 ;
!              ngroups = 5 ;
!variables:
!              float AntennaTemperature(nscans, nfovs, nchannel) ;
!              float Longitude(nscans, nfovs) ;
!              float Latitude(nscans, nfovs) ;
!              float SatelliteZenithAngle(nscans, nfovs) ;
!              float SolarZenithAngle(nscans, nfovs) ;
!              float BeamLatitude(nscans, nfovs, ngroups) ;
!              float BeamLongitude(nscans, nfovs, ngroups) ;
!              int64 BeamTime(nscans, nfovs) ;
!              int Ascending_Descending_Indicator(nscans, nfovs) ;


       program read_beamtime_dynamic

       use netcdf
       use iso_c_binding
       implicit none

       ! File and variable info
       character(len=1024) :: filename ! input netcdf filename 
       integer :: nf, jf, ibin, iargc, iret, nf_in_bin
       integer :: ncid, varid, dimid_scans, dimid_fovs
       integer :: retval
       integer :: nscans, nfovs
       integer(c_int64_t) :: epoch
       character(len=25) :: timestamp

       ! Allocate array dynamically
       integer(kind=8), allocatable :: BeamTime(:,:)
 
      nf =  iargc()
      If (nf.ne.1) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"print_BeamTime nc_file_name" 
         stop
      End If
      call getarg(1, filename) 

       ! Open NetCDF file
       retval = nf90_open(filename, NF90_NOWRITE, ncid)
       !write(*, *) "nf90_open: retval=", retval
       !if (retval /= nf90_noerr) stop "Error opening file"

       ! Get dimension IDs
       retval = nf90_inq_dimid(ncid, "nscans", dimid_scans)
       retval = nf90_inq_dimid(ncid, "nfovs", dimid_fovs)

       ! Get dimension sizes
       retval = nf90_inquire_dimension(ncid, dimid_scans, len=nscans)
       retval = nf90_inquire_dimension(ncid, dimid_fovs, len=nfovs)
       ! print *, "BeamTime dimension: nscans: ", nscans, " nfovs: ", nfovs

       ! Allocate array
       allocate(BeamTime(nfovs, nscans)) 

       ! Get variable ID
       call check(nf90_inq_varid(ncid, "BeamTime", varid)) 

       ! Read data
       call check(nf90_get_var(ncid, varid, BeamTime)) 
       !write(*, *) "nf90_getvar: retval=", retval
       !if (retval /= nf90_noerr) stop "Error reading BeamTime"

       ! Close file
       retval = nf90_close(ncid)

       !print *, "BeamTime read successfully. Size: ", nscans, " x ", nfovs
       ! BeamTime is in microsec, and starts from 1958/1/1. 
       ![tiany@rain cdr-atms-combine]$ date -u -d "1958/1/1" +%s 
       !-378691200

       Do nf=1, nscans
         epoch= BeamTime(1, nf)/1000000-378691200
         call epoch_to_datetime(epoch, timestamp)
         !write(*, *) BeamTime(1, nf), epoch, " ", trim(timestamp) 
         write(*, *)trim(timestamp) 
       End Do 

end program read_beamtime_dynamic

