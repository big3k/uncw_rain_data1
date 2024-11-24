/***************************************************************************
 *  Program Name      : rama_fcdr.c
 *  Type              : Subroutine
 *  Function          : Program reads the record of AMSU-A FCDR file 
 *  Input Files       : CICS_V00R01_AMSUA_FCDR_N15_Dyyddd.Shhmmss.Ehhmmss_B*.nc
 *			(yy: year, ddd: julian day, sttt: starting time,
 *			 ettt: ending time) 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : rama_wtcdr.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   09/05/2014      v2.0
 *************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "ESWATH.h"

/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

/**************************************************************/
size_t rama_fcdr(char *fname)
{
     int         status;
     int         ncid, grpid, varid, nscanid;
     size_t         nscanlen;
     int         iscan;

// Open NetCDF dataset
   printf("YDT: rama_fcdr.c, fname=%s\n", fname);
   status = nc_open(fname, 0, &ncid);			
   if (status != NC_NOERR) ERR(status);

// inquire dimension ID
   status = nc_inq_dimid(ncid, "nscan", &nscanid);	 
   if (status != NC_NOERR) ERR(status);

// inquire dimension length
   status = nc_inq_dimlen(ncid, nscanid, &nscanlen);	
   if (status != NC_NOERR) ERR(status);
   printf("total %d scanlines\n", nscanlen);
    
////////////////////////////////////////////////////////
// Data Fields
////////////////////////////////////////////////////////
   status = nc_inq_grp_ncid(ncid, "Data_Fields", &grpid);	
   if (status != NC_NOERR) ERR(status);

// orbital mode
   status = nc_inq_varid(grpid, "orbital_mode", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_uchar(grpid, varid, &orb_mode[0]);	
   if (status != NC_NOERR) ERR(status);

// surface type
   status = nc_inq_varid(grpid, "surface_type_a1_1", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_schar(grpid, varid, &stype_a1_1[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "surface_type_a1_2", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_schar(grpid, varid, &stype_a1_2[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "surface_type_a2", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_schar(grpid, varid, &stype_a2[0][0]);	
   if (status != NC_NOERR) ERR(status);

// earth incidence angle
   status = nc_inq_varid(grpid, "earth_angle_of_incidence_a1_1", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &lza_a1_1[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "earth_angle_of_incidence_a1_2", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &lza_a1_2[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "earth_angle_of_incidence_a2", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &lza_a2[0][0]);	
   if (status != NC_NOERR) ERR(status);

// solar zenith angle
   status = nc_inq_varid(grpid, "solar_zenith_angle", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &sza[0][0]);	
   if (status != NC_NOERR) ERR(status);

// fcdr
   status = nc_inq_varid(grpid, "fcdr_brightness_temperature_23", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &fcdr_23[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "fcdr_brightness_temperature_31", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &fcdr_31[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "fcdr_brightness_temperature_50", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &fcdr_50[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "fcdr_brightness_temperature_89", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &fcdr_89[0][0]);	
   if (status != NC_NOERR) ERR(status);

// qa prod
   status = nc_inq_varid(grpid, "flag_brightness_temperature", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_uchar(grpid, varid, &qa_prod[0][0]);	
   if (status != NC_NOERR) ERR(status);

////////////////////////////////////////////////////////
// Geolocation Time Fields
////////////////////////////////////////////////////////
   status = nc_inq_grp_ncid(ncid, "Geolocation_Time_Fields", &grpid);	
   if (status != NC_NOERR) ERR(status);

// latitude
   status = nc_inq_varid(grpid, "latitude_a1_1", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &lat_a1_1[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "latitude_a1_2", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &lat_a1_2[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "latitude_a2", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &lat_a2[0][0]);	
   if (status != NC_NOERR) ERR(status);

// longitude
   status = nc_inq_varid(grpid, "longitude_a1_1", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &lon_a1_1[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "longitude_a1_2", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &lon_a1_2[0][0]);	
   if (status != NC_NOERR) ERR(status);

   status = nc_inq_varid(grpid, "longitude_a2", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_float(grpid, varid, &lon_a2[0][0]);	
   if (status != NC_NOERR) ERR(status);

// scan time
   //for (iscan = 0; iscan < nscanlen; iscan ++) str_scantime[iscan] = malloc(20);
   status = nc_inq_varid(grpid, "scan_time", &varid);	
   if (status != NC_NOERR) ERR(status);
   //status = nc_get_var_string(grpid, varid, str_scantime);	
   status = nc_get_var_text(grpid, varid, &str_scantime[0][0]);	
   if (status != NC_NOERR) ERR(status);

// scan time since 98
   status = nc_inq_varid(grpid, "scan_time_since98", &varid);	
   if (status != NC_NOERR) ERR(status);
   status = nc_get_var_double(grpid, varid, &time_tai93[0]);	
   if (status != NC_NOERR) ERR(status);

// close netCDF dataset
   status = nc_close(ncid);       			 
   if (status != NC_NOERR) ERR(status);
 
   return nscanlen;

} /* end of rama_fcdr.c */
