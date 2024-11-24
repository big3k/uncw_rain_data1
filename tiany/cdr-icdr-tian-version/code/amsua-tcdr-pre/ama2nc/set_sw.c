/***************************************************************************
 *  Program Name      : set_sw.c
 *  Type              : Subroutine
 *  Function          : Program sets up and writes AMSU-A NETCDF TCDR file 
 *  Input Files       : None 
 *  Output Files      : NetCDF TCDR file
 *  Subroutine Called : minmax.c 
 *  Called by         : rama_wtcdr.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   9/5/2014      v2.0
 *************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "ESWATH.h"

#define NDIMS 2

/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

float *minmax();

/******************************************************************/
/*long int sw_def (char *nc_filename, short int  num_of_scan);
long int sw_wrt (char *nc_filename, short int  num_of_scan);
*/
/******************************************************************/
void set_sw(char *nc_filename, short int numscan)
{ 
   int        status;
   char       chan_name[9];

   int        ncid, x_dimid, y_dimid, c_dimid, chan_dimid, varid, grpid;
   int        dimids[NDIMS], cdimids[NDIMS], chandimids[NDIMS];
   size_t     chunks[NDIMS];
   int        shuffle, deflate, deflate_level;
   float      data_out[MAXSCANLINE_A][NUMSPOT_A];
   int        i, j, k, x, y, retval;
   float      at_scal = 1.0;
   float      at_tmp;

   time_t     rawtime;
   struct tm* timeinfo;
   char       buffer[80], buffer2[80];
   float      *latmm, *lonmm; 
   float      latv[2],lonv[2];
   double     dtime;

   float      fv = MISSING;
   const short      fv_t[] = {-1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12};
   char       fv_str[] = "greater_than_product_valid_max lower_than_product_valid_min greater_than_brightness_temperature_valid_max lower_than_brightness_temperature_valid_min CLW_indetermined RAIN_indetermined SNOW_indetermined SICE_indetermined COAST_indetermined indetermined DESERT_indetermined ELEV_indetermined";
//   char       fv_str[] = "-1: greater than product valid max; -2: lower than product valid min; -3: greater than brightness temperature valid max; -4: lower than brightness temperature valid min; -5: CLW indetermined; -6: RAIN indetermined; -7: SNOW indetermined; -8: SICE indetermined; -9: COAST indetermined; -10: indetermined; -11: DESERT indetermined; -12: ELEV indetermined";
   double     fv_zero = 0.0;
   unsigned char fv_ubyte = 255;
   int        chsize =  MAXSCANLINE_A/5;   // 270 (1350/5) 

   float      lat_lim[] = {-90.0, 90.0};
   float      lon_lim[] = {-180.0, 180.0};
   unsigned char flag_orb[] = {0, 1};
   unsigned char flag_stype[] = {0, 1, 2};
   char       str_orb[] = "northbound southbound";
   char       str_stype[] = "ocean land coast";
   Limit_A    limit_AS;
   short      slower, supper;

   /* Set chunking, shuffle, and deflate. */
   shuffle = NC_SHUFFLE;
   deflate = 1;
   deflate_level = 1;

   /* form lower and upper bound limit with scale. */
   limit_AS.RR_lower = limit_A.RR_lower * RR_SCAL;
   limit_AS.RR_upper = limit_A.RR_upper * RR_SCAL;
   limit_AS.TPW_lower = limit_A.TPW_lower * TPW_SCAL;
   limit_AS.TPW_upper = limit_A.TPW_upper * TPW_SCAL;
   limit_AS.CLW_lower = limit_A.CLW_lower * CLW_SCAL;
   limit_AS.CLW_upper = limit_A.CLW_upper * CLW_SCAL;
   limit_AS.STemp_lower = limit_A.STemp_lower * TS_SCAL;
   limit_AS.STemp_upper = limit_A.STemp_upper * TS_SCAL;
   limit_AS.Em23_lower = limit_A.Em23_lower * EM_SCAL;
   limit_AS.Em23_upper = limit_A.Em23_upper * EM_SCAL;
   limit_AS.Em31_lower = limit_A.Em31_lower * EM_SCAL;
   limit_AS.Em31_upper = limit_A.Em31_upper * EM_SCAL;
   limit_AS.Em50_lower = limit_A.Em50_lower * EM_SCAL;
   limit_AS.Em50_upper = limit_A.Em50_upper * EM_SCAL;
   limit_AS.Tsfc_lower = limit_A.Tsfc_lower * TS_SCAL;
   limit_AS.Tsfc_upper = limit_A.Tsfc_upper * TS_SCAL;


   /* Create the file. The NC_NETCDF4 parameter tells netCDF to create
    * a file in netCDF-4/HDF5 standard. */
   if ((retval = nc_create(nc_filename, NC_CLOBBER|NC_NETCDF4, &ncid)))
      ERR(retval);

   printf("YDT before defining the dimensions. chunks[0]=%d chunks[1]=%d\n",  chunks[0], chunks[1]);

   /* Define the dimensions. */
   if ((retval = nc_def_dim(ncid, "nscan", numscan, &x_dimid)))
      ERR(retval);
   if ((retval = nc_def_dim(ncid, "npixel", NUMSPOT_A, &y_dimid)))
      ERR(retval);
   if ((retval = nc_def_dim(ncid, "nchar", NCHAR, &c_dimid)))
      ERR(retval);
   if ((retval = nc_def_dim(ncid, "nchan", NUMCHAN_A, &chan_dimid)))
      ERR(retval);

   /* Set up variabe data. */
   dimids[0] = x_dimid;
   dimids[1] = y_dimid;
   cdimids[0] = x_dimid;
   cdimids[1] = c_dimid;
   chandimids[0] = x_dimid;
   chandimids[1] = chan_dimid;
   chunks[0] = MAXSCANLINE_A/5;
   chunks[1] = NUMSPOT_A/5;

   printf("YDT after defining the dimensions. chunks[0]=%d chunks[1]=%d\n",  chunks[0], chunks[1]);
/*--------------------------------------------------------*
 * Global attributes 
 *--------------------------------------------------------*/
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "Conventions", 
                          6, "CF-1.6")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "Metadata_Conventions", 
                          63, "CF-1.6, Unidata Dataset Discovery v1.0, NOAA CDR v1.0, GDS v2.0")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "standard_name_vocabulary", 
                          49, "CF Standard Name Table (Version 34, 13 June 2016)")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "id", 
                          sizeof(nc_fname), nc_fname)))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "naming_authority", 
                          13, "gov.noaa.ncdc")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "metadata_link", 
                          20, "gov.noaa.ncdc:C00982")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "title", 
                          25, "CICS Version-1 AMSUA TCDR")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "product_version", 
                          18, "V01R01-preliminary")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "date_issued", 
                          3, "TBD")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "summary", 
                          384, "CDRs for AMSU window channels and corresponding hydrological products are vital for the climate community. The corrections applied to the CDRs include geolocation correction, AMSU-A scan bias corrections, and intersatellte calibration. The monthly initially submitted “preliminary” CDR data for the same time period will be replaced by the “final” CDR data on an annual basis.")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "keywords", 
                          73, "EARTH SCIENCE > SPECTRAL/ENGINEERING > MICROWAVE > BRIGHTNESS TEMPERATURE")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "keywords_vocabulary", 
                          78, "NASA Global Change Master Directory (GCMD) Earth Science Keywords, Version 8.1")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "platform", 
                          62, platform)))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "sensor", 
                          45, "AMSU-A > Advanced Microwave Sounding Unit - A")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "cdm_data_type", 
                          5, "Swath")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "cdr_program", 
                          56, "NOAA Climate Data Record Program for satellites, FY 2016")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "cdr_variable", 
                          48, "TPW, CLW, SIce, T_sfc, Emis_23, Emis_31, Emis_89")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "source", 
                          73, hdf_fname)))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "references", 
                          99, "Operational L2 data are available at NOAA Comprehensive Large Array-data Stewardship System (CLASS)")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "history", 
                          3, "TBD")))
      ERR(retval);

   time(&rawtime);
   timeinfo = gmtime(&rawtime);
   strftime(buffer,80,"%Y-%m-%dT%H:%M:%S",timeinfo);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "date_created", 
                          19, buffer)))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "creator_name", 
                          26, "Huan Meng, Ralph R Ferraro")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "creator_url", 
                          38, "http://cics.umd.edu/AMSU-CDR/home.html")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "creator_email", 
                          44, "Huan.Meng@noaa.gov, Ralph.R.Ferraro@noaa.gov")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "institution", 
                          147, "DOC/NOAA/NESDIS/STAR/CoRP > Cooperative Research Program, Center for Satellite Applications and Research, NESDIS, NOAA, U.S. Department of Commerce")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "processing_level", 
                          12, "NOAA level 2")))
      ERR(retval);

   latmm = minmax(lat_a1_1,numscan,NUMSPOT_A);
   latv[0] = *latmm;
   latv[1] = *(latmm+1);
   lonmm = minmax(lon_a1_1,numscan,NUMSPOT_A);
   lonv[0] = *lonmm;
   lonv[1] = *(lonmm+1);
   if ((retval = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_lat_min", NC_FLOAT,
                          1, &latv[0])))
      ERR(retval);
   if ((retval = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_lat_max", NC_FLOAT,
                          1, &latv[1])))
      ERR(retval);
   if ((retval = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_lon_min", NC_FLOAT,
                          1, &lonv[0])))
      ERR(retval);
   if ((retval = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_lon_max", NC_FLOAT,
                          1, &lonv[1])))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "geospatial_lat_units", 
                          13, "degrees_north")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "geospatial_lon_units", 
                          12, "degrees_east")))
      ERR(retval);

   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "spatial_resolution", 
                          42, "48km X 48km at nadir, 150km X 80km at limb")))
      ERR(retval);

   sscanf(str_scantime[0],"%19c",buffer);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "time_coverage_start", 
                          19, buffer)))
      ERR(retval);

   sscanf(str_scantime[numscan-1],"%19c",buffer);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "time_coverage_end", 
                          19, buffer)))
      ERR(retval);

   dtime = time_tai93[numscan-1] - time_tai93[0];
   sprintf(buffer2,"P%.0lfS",dtime);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "time_coverage_duration", 
                          6, buffer2)))
      ERR(retval);

   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "license", 
                          32, "No restrictions on access or use")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "contributor_name", 
                          3, "TBD")))
      ERR(retval);
   if ((retval = nc_put_att_text(ncid, NC_GLOBAL, "contributor_role", 
                          3, "TBD")))
      ERR(retval);

   printf("YDT Group 1. chunks[0]=%d chunks[1]=%d\n",  chunks[0], chunks[1]);
/*--------------------------------------------------------*
 * Group 1: geolocation field 
 *--------------------------------------------------------*/
   if ((retval = nc_def_grp(ncid, "Geolocation_Time_Fields", &grpid)))
      ERR (retval);

/*--------------------------------------------------------*
 * Time fields 
 *--------------------------------------------------------*/
// scan_time
   if ((retval = nc_def_var(grpid, "scan_time", NC_CHAR, NDIMS,
                          cdimids, &varid)))
      ERR(retval);
   //if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
   //                       4, "time")))
   //   ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          71, "Scan start time (UTC) in ISO8601 date/time (YYYY-MM-DDTHH-MM-SS) format")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "_FillValue",
                          1, "0")))
      ERR(retval);
   if (numscan > chsize) 
   {
   printf("YDT here 1. chunks[0]=%d chunks[1]=%d\n",  chunks[0], chunks[1]);
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   printf("YDT here 2. chunks[0]=%d chunks[1]=%d\n",  chunks[0], chunks[1]);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
   }
   if ((retval = nc_put_var_text(grpid, varid, &str_scantime[0][0])))
      ERR(retval);

// scan_time_since93
   if ((retval = nc_def_var(grpid, "scan_time_since98", NC_DOUBLE, 1,
                          &x_dimid, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
                          4, "time")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          60, "Scan start time (UTC) in a referenced or elapsed time format")))
      ERR(retval);
   if ((retval = nc_put_att_double(grpid, varid, "flag_values", NC_DOUBLE,
                          1, &fv_zero)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          33, "seconds since 1998-01-01T00:00:00")))
      ERR(retval);

   if (numscan > chsize) 
   {
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
   }
   if ((retval = nc_put_var_double(grpid, varid, &time_tai93[0])))
      ERR(retval);

/*--------------------------------------------------------*
 * Lat/lon fields 
 *--------------------------------------------------------*/
// latitude_a2
   if ((retval = nc_def_var(grpid, "latitude", NC_FLOAT, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
                          8, "latitude")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          19, "Latitude for AMSU-A")))
      ERR(retval);
   if ((retval = nc_put_att_float(grpid, varid, "valid_min", NC_FLOAT,
                          1, &lat_lim[0])))
      ERR(retval);
   if ((retval = nc_put_att_float(grpid, varid, "valid_max", NC_FLOAT,
                          1, &lat_lim[1])))
      ERR(retval);
   if ((retval = nc_put_att_float(grpid, varid, "flag_values", NC_FLOAT,
                          1, &fv)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          13, "degrees_north")))
      ERR(retval);
   if (numscan > chsize) 
   {
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
   }
   if ((retval = nc_put_var_float(grpid, varid, &lat_a2[0][0])))
      ERR(retval);

// longitude_a2
   if ((retval = nc_def_var(grpid, "longitude", NC_FLOAT, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
                          9, "longitude")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          20, "Longitude for AMSU-A")))
      ERR(retval);
   if ((retval = nc_put_att_float(grpid, varid, "valid_min", NC_FLOAT,
                          1, &lon_lim[0])))
      ERR(retval);
   if ((retval = nc_put_att_float(grpid, varid, "valid_max", NC_FLOAT,
                          1, &lon_lim[1])))
      ERR(retval);
   if ((retval = nc_put_att_float(grpid, varid, "flag_values", NC_FLOAT,
                          1, &fv)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          13, "degrees_east")))
      ERR(retval);
   if (numscan > chsize) 
   {
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
   }
   if ((retval = nc_put_var_float(grpid, varid, &lon_a2[0][0])))
      ERR(retval);

   printf("YDT Group 2. chunks[0]=%d chunks[1]=%d\n",  chunks[0], chunks[1]);
/*--------------------------------------------------------*
 * Group 2: data field 
 *--------------------------------------------------------*/
   if ((retval = nc_def_grp(ncid, "Data_Fields", &grpid)))
      ERR (retval);
 
/*--------------------------------------------------------*
 * Orbit mode data field 
 *--------------------------------------------------------*/
   if ((retval = nc_def_var(grpid, "orbital_mode", NC_BYTE, 1,
                          &x_dimid, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          19, "satellite direction")))
      ERR(retval);
   if ((retval = nc_put_att_schar(grpid, varid, "flag_values", NC_BYTE,
                          2, &flag_orb[0])))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "flag_meanings", 
                          sizeof(str_orb), str_orb)))
      ERR(retval);
   if ((retval = nc_put_att_schar(grpid, varid, "flag_values", NC_BYTE,
                          1, &fv_ubyte)))
      ERR(retval);
   printf("YDT here -100. chunks[0]=%d chunks[1]=%d\n",  chunks[0], chunks[1]);
   printf("YDT here -99. numscan=%d chsize=%d\n",  numscan, chsize);
   if (numscan > chsize) 
   {
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
      printf("YDT here -90. chunks[0]=%d chunks[1]=%d\n",  chunks[0], chunks[1]);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
   }
   if ((retval = nc_put_var_schar(grpid, varid, &orb_mode[0])))
      ERR(retval);

/*--------------------------------------------------------*
 * Surface type data field 
 *--------------------------------------------------------*/
   if ((retval = nc_def_var(grpid, "surface_type", NC_BYTE, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          12, "surface type")))
      ERR(retval);
   if ((retval = nc_put_att_schar(grpid, varid, "flag_values", NC_BYTE,
                          3, &flag_stype[0])))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "flag_meanings", 
                          sizeof(str_stype), str_stype)))
      ERR(retval);
   if ((retval = nc_put_var_schar(grpid, varid, &stype_a2[0][0])))
      ERR(retval);


/*--------------------------------------------------------*
 * Angle data fields 
 *--------------------------------------------------------*/
// earth_incidence_angle_a2
   if ((retval = nc_def_var(grpid, "earth_angle_of_incidence", NC_FLOAT, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          32, "earth angle of incidence for AMSU-A")))
      ERR(retval);
   if ((retval = nc_put_att_float(grpid, varid, "flag_values", NC_FLOAT,
                          1, &fv)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          6, "degree")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "coordinates", 
                          18, "latitude longitude")))
      ERR(retval);
   if (numscan > chsize) 
   {
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
   }
   if ((retval = nc_put_var_float(grpid, varid, &lza_a2[0][0])))
      ERR(retval);

   printf("YDT here -80. numscan=%d chsize=%d\n",  numscan, chsize);

/*--------------------------------------------------------*
 * Product Quality Flag data field 
 *--------------------------------------------------------*/
   if ((retval = nc_def_var(grpid, "flag_brightness_temperature", NC_UBYTE, NDIMS,
                          chandimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          31, "instrument/channel quality flag")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "comment", 
                          643, "The 8-bit quality flags are channel-specific for the brightness temperatures used to derive these products. Users are advised not to use any scans for which the highest bit (bit 7) is set to 1. The lowest two bits (bits 0 and 1) are not used. The meanings of the bits from the MSB (bit 7) to the LSB (bit 0), if set to 1, are: bit 7 = Do not use scan for product generation; bit 6 = Calibration error; bit 5 = Time sequence error; bit 4 = Earth location questionable; bit 3 = Brightness temperature out of range; bit 2 = Lunar contamination warning; bit 1~0 = zero fill. If any bit is set to 0, then there is no correspondent error or warning.")))
      ERR(retval);
   if ((retval = nc_put_att_uchar(grpid, varid, "flag_masks", NC_UBYTE,
                          2, &flag_orb[0])))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "flag_meanings", 
                          16, "good problematic")))
      ERR(retval);
   if ((retval = nc_put_var_uchar(grpid, varid, &qa_prod[0][0])))
      ERR(retval);

/*--------------------------------------------------------*
 * TPW data field 
 *--------------------------------------------------------*/
   if ((retval = nc_def_var(grpid, "TPW", NC_SHORT, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          36, "NOAA CDR of total precipitable water")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
                          30, "atmosphere_water_vapor_content")))
      ERR(retval);
   at_tmp = 1/TPW_SCAL;
   slower = (short) limit_AS.TPW_lower;
   supper = (short) limit_AS.TPW_upper;
   if ((retval = nc_put_att_float(grpid, varid, "scale_factor", NC_FLOAT,
                          1, &at_tmp)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_min", NC_SHORT,
                          1, &slower)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_max", NC_SHORT,
                          1, &supper)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "flag_values", NC_SHORT,
                          12, &fv_t[0])))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "flag_meanings", 
                          sizeof(fv_str), fv_str)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          6, "kg m-2")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "coordinates", 
                          18, "latitude longitude")))
      ERR(retval);
   printf("YDT here -70. numscan=%d chsize=%d\n",  numscan, chsize);
  if (numscan > chsize) {  //YDT 11/24/2024
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
  } // end YDT
   if ((retval = nc_put_var_short(grpid, varid, &tpw[0][0])))
      ERR(retval);

/*--------------------------------------------------------*
 * CLW data field 
 *--------------------------------------------------------*/
   if ((retval = nc_def_var(grpid, "CLW", NC_SHORT, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          30, "NOAA CDR of cloud liquid water")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
                          37, "atmosphere_cloud_liquid_water_content")))
      ERR(retval);
   at_tmp = 1/CLW_SCAL;
   slower = (short) limit_AS.CLW_lower;
   supper = (short) limit_AS.CLW_upper;
   if ((retval = nc_put_att_float(grpid, varid, "scale_factor", NC_FLOAT,
                          1, &at_tmp)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_min", NC_SHORT,
                          1, &slower)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_max", NC_SHORT,
                          1, &supper)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "flag_values", NC_SHORT,
                          12, &fv_t[0])))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "flag_meanings", 
                          sizeof(fv_str), fv_str)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          6, "kg m-2")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "coordinates", 
                          18, "latitude longitude")))
      ERR(retval);
   printf("YDT here -60. numscan=%d chsize=%d\n",  numscan, chsize);
  if (numscan > chsize) {  //YDT 11/24/2024
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
  } // end YDT
   if ((retval = nc_put_var_short(grpid, varid, &clw[0][0])))
      ERR(retval);

/*--------------------------------------------------------*
 * SICE data field 
 *--------------------------------------------------------*/
   if ((retval = nc_def_var(grpid, "SIce", NC_SHORT, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          19, "NOAA CDR of sea ice")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
                          21, "sea_ice_area_fraction")))
      ERR(retval);
   slower = (short) limit_A.SIce_lower;
   supper = (short) limit_A.SIce_upper;
   if ((retval = nc_put_att_short(grpid, varid, "valid_min", NC_SHORT,
                          1, &slower)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_max", NC_SHORT,
                          1, &supper)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "flag_values", NC_SHORT,
                          12, &fv_t[0])))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "flag_meanings", 
                          sizeof(fv_str), fv_str)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          1, "1")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "coordinates", 
                          18, "latitude longitude")))
      ERR(retval);
   printf("YDT here -50. numscan=%d chsize=%d\n",  numscan, chsize);
  if (numscan > chsize) {  //YDT 11/24/2024
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
  } // end YDT
   if ((retval = nc_put_var_short(grpid, varid, &sice[0][0])))
      ERR(retval);

/*--------------------------------------------------------*
 * STEMP data field 
 *--------------------------------------------------------*/
   if ((retval = nc_def_var(grpid, "T_sfc", NC_SHORT, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          31, "NOAA CDR of surface temperature")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
                          30, "surface_temperature_where_land")))
      ERR(retval);
   at_tmp = 1/TS_SCAL;
   slower = (short) limit_AS.Tsfc_lower;
   supper = (short) limit_AS.Tsfc_upper;
   if ((retval = nc_put_att_float(grpid, varid, "scale_factor", NC_FLOAT,
                          1, &at_tmp)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_min", NC_SHORT,
                          1, &slower)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_max", NC_SHORT,
                          1, &supper)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "flag_values", NC_SHORT,
                          12, &fv_t[0])))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "flag_meanings", 
                          sizeof(fv_str), fv_str)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          1, "K")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "coordinates", 
                          18, "latitude longitude")))
      ERR(retval);
   printf("YDT here -40. numscan=%d chsize=%d\n",  numscan, chsize);
  if (numscan > chsize) {  //YDT 11/24/2024
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
  } // end YDT
   if ((retval = nc_put_var_short(grpid, varid, &ts[0][0])))
      ERR(retval);

/*--------------------------------------------------------*
 * EMISSIVITY data field 
 *--------------------------------------------------------*/
   if ((retval = nc_def_var(grpid, "Emis_23", NC_SHORT, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          32, "NOAA CDR of emissivity of 23 GHz")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
                          28, "surface_microwave_emissivity")))
      ERR(retval);
   at_tmp = 1/EM_SCAL;
   slower = (short) limit_AS.Em23_lower;
   supper = (short) limit_AS.Em23_upper;
   if ((retval = nc_put_att_float(grpid, varid, "scale_factor", NC_FLOAT,
                          1, &at_tmp)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_min", NC_SHORT,
                          1, &slower)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_max", NC_SHORT,
                          1, &supper)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "flag_values", NC_SHORT,
                          12, &fv_t[0])))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "flag_meanings", 
                          sizeof(fv_str), fv_str)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          1, "1")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "coordinates", 
                          18, "latitude longitude")))
      ERR(retval);
   printf("YDT here -30. numscan=%d chsize=%d\n",  numscan, chsize);
  if (numscan > chsize) {  //YDT 11/24/2024
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
  }
   if ((retval = nc_put_var_short(grpid, varid, &em1[0][0])))
      ERR(retval);


   if ((retval = nc_def_var(grpid, "Emis_31", NC_SHORT, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          32, "NOAA CDR of emissivity of 31 GHz")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
                          28, "surface_microwave_emissivity")))
      ERR(retval);
   if ((retval = nc_put_att_float(grpid, varid, "scale_factor", NC_FLOAT,
                          1, &at_tmp)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_min", NC_SHORT,
                          1, &slower)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_max", NC_SHORT,
                          1, &supper)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "flag_values", NC_SHORT,
                          12, &fv_t[0])))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "flag_meanings", 
                          sizeof(fv_str), fv_str)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          1, "1")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "coordinates", 
                          18, "latitude longitude")))
      ERR(retval);
   printf("YDT here -20. numscan=%d chsize=%d\n",  numscan, chsize);
  if (numscan > chsize) {  //YDT 11/24/2024
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
  } 
   if ((retval = nc_put_var_short(grpid, varid, &em2[0][0])))
      ERR(retval);


   if ((retval = nc_def_var(grpid, "Emis_50", NC_SHORT, NDIMS,
                          dimids, &varid)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "long_name", 
                          32, "NOAA CDR of emissivity of 50 GHz")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "standard_name", 
                          28, "surface_microwave_emissivity")))
      ERR(retval);
   if ((retval = nc_put_att_float(grpid, varid, "scale_factor", NC_FLOAT,
                          1, &at_tmp)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_min", NC_SHORT,
                          1, &slower)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "valid_max", NC_SHORT,
                          1, &supper)))
      ERR(retval);
   if ((retval = nc_put_att_short(grpid, varid, "flag_values", NC_SHORT,
                          12, &fv_t[0])))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "flag_meanings", 
                          sizeof(fv_str), fv_str)))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "units", 
                          1, "1")))
      ERR(retval);
   if ((retval = nc_put_att_text(grpid, varid, "coordinates", 
                          18, "latitude longitude")))
      ERR(retval);
   printf("YDT here -10. numscan=%d chsize=%d\n",  numscan, chsize);
  if (numscan > chsize) {  //YDT 11/24/2024
   if ((retval = nc_def_var_chunking(grpid, varid, 0, &chunks[0])))
      ERR(retval);
   if ((retval = nc_def_var_deflate(grpid, varid, shuffle, deflate,
                                  deflate_level)))
      ERR(retval);
  } 
   if ((retval = nc_put_var_short(grpid, varid, &em3[0][0])))
      ERR(retval);

   /* Close the file. */
   if ((retval = nc_close(ncid)))
      ERR(retval);

   printf("*** SUCCESS writing example file %s!\n", nc_filename);
}/*  end of set_sw.c  */
