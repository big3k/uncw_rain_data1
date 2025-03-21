/***************************************************************************
 *  Program Name      : gnrt_fname.c
 *  Type              : Subroutine
 *  Function          : Program generates AMSU-A swath file name 
 *  Input Files       : None 
 *  Output Files      : SWATH_MOA_AMSUA_Syyddd_sttttt_Eyyddd_ettttt.nc
 *			(yy: year, ddd: julian day, sttttt: starting time,
 *			 ettttt: ending time)
 *  Subroutine Called : None 
 *  Called by         : rama_wfcdr.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *   3/20/2025      YDT               see below: 
 *
 *   # code modified to generate nc file name from: 
 * #   1          2           3     4    5   6       7      8
 * # CICS_V01R01-preliminary_AMSUA_TCDR_N19_D24216_S061209_E080313_B7982829.nc
 *                                           yyddd  hhmmss  hhmmss
 * # to
 * # HYDRO-CDR_v02r00_TCDR_AMSUA_N19_s202410312308_e202411010100_c20250127.nc
 *                                    yyyymmddhhmm  yyyymmddhhmm  yyyymmdd

 *   
 *************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "ESWATH.h"
#include <string.h>
#include <time.h> 

/*****************************************************************/
//char* gnrt_fname(char* nc_filename)
void gnrt_fname(char* nc_filename, char direct[], char fname_1b[], short int numscan)
{

   char     temp[80];

   char     nc[] = {".nc"};
   char     *token;
   char     *yd;
   char     *search = ".";
   int      i;
   char     buffer[80], sstr[80], estr[80];
   int      hh, mm, ss; 

   //YDT:vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
   time_t t;
   struct tm *tm_info;
   char     cdate_str[9]; //yyyymmdd
   int      hh0, mm0, ss0; 
   int      hh1, mm1, ss1; 
   int sec0, sec1;

    // Get current time
    time(&t);
    tm_info = localtime(&t);

    // Format date
    strftime(cdate_str, sizeof(cdate_str), "%Y%m%d", tm_info);
    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   switch (hblock.sat_id)
   {
      case 4: 
         sprintf(sid, "N15"); 
         snum = 0;
         sprintf(platform, "NOAA-15 > National Oceanic and Atmospheric Administration - 15");
         break;
      case 2: 
         sprintf(sid, "N16"); 
         snum = 1;
         sprintf(platform, "NOAA-16 > National Oceanic and Atmospheric Administration - 16");
         break;
      case 6: 
         sprintf(sid, "N17"); 
         snum = 2;
         sprintf(platform, "NOAA-17 > National Oceanic and Atmospheric Administration - 17");
         break;
      case 7: 
         sprintf(sid, "N18"); 
         snum = 3;
         sprintf(platform, "NOAA-18 > National Oceanic and Atmospheric Administration - 18");
         break;
      case 8: 
         sprintf(sid, "N19"); 
         snum = 5;
         sprintf(platform, "NOAA-19 > National Oceanic and Atmospheric Administration - 19");
         break;
      case 12: 
         sprintf(sid, "MOA"); 
         snum = 4;
         sprintf(platform, "MetOp-A > Meteorological Operational satellite programme - A");
         break;
      default: printf("wrong satellite id\n"); break;
   }

   yd = strtok(fname_1b, search);
   for (i=0; i<3;i++) 
      yd = strtok(NULL, search); 
   for (i=0; i<3;i++) 
      token = strtok(NULL, search); 

   //YDT -- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
   //YDT assuming yd="D24123" 

    // Extract substrings
    char yy_str[3] = {yd[1], yd[2], '\0'};     // "24"
    char day_str[4] = {yd[3], yd[4], yd[5], '\0'}; // "123"

    // Convert to integers
    int year = 2000 + atoi(yy_str);  // e.g., 24 → 2024
    int day_of_year = atoi(day_str); // e.g., 123

    // Set tm struct
    struct tm date = {0};
    date.tm_year = year - 1900; // tm_year is years since 1900
    date.tm_mday = day_of_year;

    // Normalize using mktime (handles day_of_year correctly)
    mktime(&date);

    // Output formatted as yyyymmdd
    char syyyymmdd[9];
    char eyyyymmdd[9];

    strftime(syyyymmdd, sizeof(syyyymmdd), "%Y%m%d", &date);
   
   sscanf(str_scantime[0],"%11c%2d:%2d:%2d",buffer,&hh0,&mm0,&ss0);
   sec0=hh0*60*60+mm0*60+ss0;   
   sscanf(str_scantime[numscan-1],"%11c%2d:%2d:%2d",buffer,&hh1,&mm1,&ss1);
   sec1=hh1*60*60+mm1*60+ss1;   
   
   if ( sec1 > sec0 ) { // no day jump 
       strcpy(eyyyymmdd, syyyymmdd); 
   } else { // jump one day  
       struct tm date = {0};
       date.tm_year = year - 1900; // tm_year is years since 1900
       date.tm_mday = day_of_year + 1;
       mktime(&date);
       strftime(eyyyymmdd, sizeof(eyyyymmdd), "%Y%m%d", &date);
   }

   // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   /* original 
   sscanf(str_scantime[0],"%11c%2d:%2d:%2d",buffer,&hh,&mm,&ss);
   sprintf(sstr,"S%.2d%.2d%.2d",hh,mm,ss);
   sscanf(str_scantime[numscan-1],"%11c%2d:%2d:%2d",buffer,&hh,&mm,&ss);
   sprintf(estr,"E%.2d%.2d%.2d",hh,mm,ss);
   */ 

   //YDT sprintf(temp,"CICS_V01R01-preliminary_AMSUA_FCDR_%s_%s_%s_%s_%s", sid, yd, sstr, estr, token);
   sprintf(temp,"HYDRO-CDR_v02r00_FCDR_AMSUA_%s_s%s%.2d%.2d_e%s%.2d%.2d_c%s", sid, syyyymmdd, hh0, mm0, eyyyymmdd, hh1, mm1, cdate_str); 
//   sprintf(temp,"CICS_V01R00_AMSUA_FCDR_%s_%s_S%.6ld_E%.6ld_%s", sid, yd, stime, etime, token);
   strcpy(nc_filename,direct);

   strcpy(nc_fname,temp);
   strcat(nc_fname,nc);

   strcat(nc_filename, nc_fname);

   printf("gnrt_fname/filename: %s\n",nc_filename);
   printf("gnrt_fname/filename: %s\n",nc_fname);
   
} /* end of gnrt_fname.c */                             
