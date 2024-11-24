/***************************************************************************
 *  Program Name      : gnrt_fname.c
 *  Type              : Subroutine
 *  Function          : Program generates AMSU-A TCDR file name 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : rama_wtcdr.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   9/5/2014      v2.0
 *************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "ESWATH.h"
#include <string.h>

/*****************************************************************/
//char* gnrt_fname(char* nc_filename)
void gnrt_fname(char* nc_filename, char direct[], char fname_fcdr[])
{

   strcpy(nc_fname,fname_fcdr);
   nc_fname[30] = 'T';
   sid = strndup(nc_fname + 35, 3);
   
   if (strcmp(sid,"N15") == 0)
      sprintf(platform, "NOAA-15 > National Oceanic and Atmospheric Administration - 15");
   else if (strcmp(sid,"N16") == 0)
      sprintf(platform, "NOAA-16 > National Oceanic and Atmospheric Administration - 16");
   else if (strcmp(sid,"N17") == 0)
      sprintf(platform, "NOAA-17 > National Oceanic and Atmospheric Administration - 17");
   else if (strcmp(sid,"N18") == 0)
      sprintf(platform, "NOAA-18 > National Oceanic and Atmospheric Administration - 18");
   else if (strcmp(sid,"N19") == 0)
      sprintf(platform, "NOAA-19 > National Oceanic and Atmospheric Administration - 19");
   else if (strcmp(sid,"MOA") == 0)
      sprintf(platform, "MetOp-A > Meteorological Operational satellite programme - A");
   else
      printf("wrong satellite id\n"); 
      
//   printf("gnrt/platform is %s.\n", platform); 

   strcpy(nc_filename,direct);
   strcat(nc_filename, nc_fname);

//   printf("gnrt_fname/filename: %s\n",nc_filename);
//   printf("gnrt_fname/filename: %s\n",nc_fname);
   
} /* end of gnrt_fname.c */                             
