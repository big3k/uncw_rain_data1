
./compile.sh 

./atms_to_fcdr /data1/tiany/cdr-atms-combine/output/2012/20120303/NPP_ATMS_d20120303_t1401193_e1542389_b001805.nc test2.nc

exit
./atms_to_fcdr /data1/tiany/cdr-atms-combine/output/2012/20120303/NPP_ATMS_d20120303_t0029086_e0210576_b001797.nc test.nc 

exit 

-------------------------------------------------------------------------------------------------------

(base) [tiany@itsrain 20120303]$ ncdump -h /data1/tiany/cdr-atms-combine/output/2012/20120303/NPP_ATMS_d20120303_t0029086_e0210576_b001797.nc
netcdf NPP_ATMS_d20120303_t0029086_e0210576_b001797 {
dimensions:
        nchannel = 22 ;
        nfovs = 96 ;
        nscans = 2292 ;
        ngroups = 5 ;
variables:
        float AntennaTemperature(nscans, nfovs, nchannel) ;
        float Longitude(nscans, nfovs) ;
        float Latitude(nscans, nfovs) ;
        float SatelliteZenithAngle(nscans, nfovs) ;
        float SolarZenithAngle(nscans, nfovs) ;
        float BeamLatitude(nscans, nfovs, ngroups) ;
        float BeamLongitude(nscans, nfovs, ngroups) ;
        int64 BeamTime(nscans, nfovs) ;
        int Ascending_Descending_Indicator(nscans, nfovs) ;
}

