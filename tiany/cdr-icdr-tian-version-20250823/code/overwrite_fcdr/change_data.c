/* YDT 8/26/2025 
 *
 * Replace the four data arrays with values given in a .dat file 
 *      float fcdr_brightness_temperature_23(nscan, npixel) ;
 *      float fcdr_brightness_temperature_31(nscan, npixel) ;
 *      float fcdr_brightness_temperature_50(nscan, npixel) ;
 *      float fcdr_brightness_temperature_89(nscan, npixel) ;

 * Usage: %s <netcdf_file> <new_tb23_file> <new_tb31_file> <new_tb50_file> <new_tb89_file> 
 *
 * To compile: 
 * gcc change_data.c -o change_data -lnetcdf
 *
 * Note: have to use netcdf4 interfaces. 
 *
 * Usage example 
 * ./change_data CICS_V01R01-preliminary_AMSUA_FCDR_N19_D24216_S125729_E144713_B7983233.nc \
 *   NSS.AMAX.NP.D24216.S1257.E1447.B7983233.SV-tb23.dat \
 *   NSS.AMAX.NP.D24216.S1257.E1447.B7983233.SV-tb31.dat \
 *   NSS.AMAX.NP.D24216.S1257.E1447.B7983233.SV-tb50.dat \
 *   NSS.AMAX.NP.D24216.S1257.E1447.B7983233.SV-tb89.dat
*/ 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netcdf.h>

#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(2);}
#define VAR_TB23 "fcdr_brightness_temperature_23"
#define VAR_TB31 "fcdr_brightness_temperature_31"
#define VAR_TB50 "fcdr_brightness_temperature_50"
#define VAR_TB89 "fcdr_brightness_temperature_89"

int main(int argc, char *argv[]) {
    if (argc != 6) {
        printf("Usage: %s <netcdf_file> <new_tb23_file> <new_tb31_file> <new_tb50_file> <new_tb89_file>\n", argv[0]);
        return 1;
    }

    const char *ncfilename = argv[1];
    const char *tb23f = argv[2];
    const char *tb31f = argv[3];
    const char *tb50f = argv[4];
    const char *tb89f = argv[5];
    int dimid_nscan, dimid_npixel;
    size_t nscan, npixel;
    int ncid; 
    int retval;
    int  varid23, varid31, varid50, varid89; 

    // Open NetCDF file to read the dimensions of the data 
    if ((retval = nc_open(ncfilename, NC_NOWRITE, &ncid))) {
        printf("Error opening file: %s\n", nc_strerror(retval));
        return retval;
    }

   // Look up dimension IDs by name
    if ((retval = nc_inq_dimid(ncid, "nscan", &dimid_nscan)))
        ERR(retval);
    if ((retval = nc_inq_dimid(ncid, "npixel", &dimid_npixel)))
        ERR(retval);

    // Get dimension lengths
    if ((retval = nc_inq_dimlen(ncid, dimid_nscan, &nscan)))
        ERR(retval);
    if ((retval = nc_inq_dimlen(ncid, dimid_npixel, &npixel)))
        ERR(retval);

    // Print them out
    printf("nscan  = %zu\n", nscan);
    printf("npixel = %zu\n", npixel);

    // Close file
    if ((retval = nc_close(ncid)))
        ERR(retval);

    size_t total = nscan * npixel;

    // --- Step 2: allocate memory for 4 channels
    float *tb23 = (float *) malloc(total * sizeof(float));
    float *tb31 = (float *) malloc(total * sizeof(float));
    float *tb50 = (float *) malloc(total * sizeof(float));
    float *tb89 = (float *) malloc(total * sizeof(float));
    if (!tb23 || !tb31 || !tb50 || !tb89) {
        fprintf(stderr, "Memory allocation failed\n");
        return 2;
    }

  // --- Step 3: helper to read a binary file
    FILE *fp;
    if (!(fp = fopen(tb23f, "rb"))) { perror("open tb23"); exit(1); }
    fread(tb23, sizeof(float), total, fp); fclose(fp);

    if (!(fp = fopen(tb31f, "rb"))) { perror("open tb31"); exit(1); }
    fread(tb31, sizeof(float), total, fp); fclose(fp);

    if (!(fp = fopen(tb50f, "rb"))) { perror("open tb50"); exit(1); }
    fread(tb50, sizeof(float), total, fp); fclose(fp);

    if (!(fp = fopen(tb89f, "rb"))) { perror("open tb89"); exit(1); }
    fread(tb89, sizeof(float), total, fp); fclose(fp);

    // Open file for writing
    if ((retval = nc_open(ncfilename, NC_WRITE, &ncid)))
        ERR(retval);

    int grp_id;
    nc_inq_ncid(ncid, "Data_Fields", &grp_id);
    printf("Find var ids %s\n", VAR_TB23);
    // Get variable IDs
    if ((retval = nc_inq_varid(grp_id, VAR_TB23, &varid23))) ERR(retval);
    printf("Find var ids %s\n", VAR_TB31);
    if ((retval = nc_inq_varid(grp_id, VAR_TB31, &varid31))) ERR(retval);
    printf("Find var ids %s\n", VAR_TB50);
    if ((retval = nc_inq_varid(grp_id, VAR_TB50, &varid50))) ERR(retval);
    printf("Find var ids %s\n", VAR_TB89);
    if ((retval = nc_inq_varid(grp_id, VAR_TB89, &varid89))) ERR(retval);

    printf("Overwriting data\n");  
    // Write full arrays (they are contiguous so use tbXX[0])
    if ((retval = nc_put_var_float(grp_id, varid23, tb23))) ERR(retval);
    if ((retval = nc_put_var_float(grp_id, varid31, tb31))) ERR(retval);
    if ((retval = nc_put_var_float(grp_id, varid50, tb50))) ERR(retval);
    if ((retval = nc_put_var_float(grp_id, varid89, tb89))) ERR(retval);

    // Close file
    if ((retval = nc_close(ncid)))
        ERR(retval);

    printf("Variables successfully overwritten in %s\n", ncfilename);
   
}
    
