
9/7/2025

Step 0: 
===============================================================================================
 Programs in ../cdr-atms-combine combine the ATMS orbits by orbit number (the "_bnnnnn_" field). 
 Sample output of this step: 
  
 /data1/tiany/cdr-atms-combine/output/2012/20120303/NPP_ATMS_d20120303_t0029086_e0210576_b001797.nc

(base) [tiany@itsrain cdr-atms-combine-to-fcdr]$ ncdump -h /data1/tiany/cdr-atms-combine/output/2012/20120303/NPP_ATMS_d20120303_t0029086_e0210576_b001797.nc
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


Step 1:
===============================================================================================
 In this directory we have code to read the output and save the four amsu-a like channels in netcdf. 




 
