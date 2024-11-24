/**************************************************************************
*@***h* AMSU-A Hydrological TCDR/AMA2NC_TCDR.c
*
* NAME
*   AMA2NC_TCDR.c
*
* PURPOSE
*   Main program to generate AMSU-A Hydrological TCDR
*
* DESCRIPTION
*   Program reads AMSU-A FCDR data, generates products
*   and creates AMSU-A TCDR SWATH file (NETCDF4 format)
*
* Input Files 
*   None 
*
* Output Files
*   None 
*
* Subroutine Called 
*   openfs.c, read_systim.c, read_parm.c, rama_wtcdr.c
*
* Called by
*   None
*
* AUTHOR
*   Huan Meng,  NOAA/NESDIS/STAR/SCSB
*   Wenze Yang, UMD/ESSIC/CICS
*
* COPYRIGHT
*   THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC
*   DOMAIN AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE. THEY ARE 
*   FINISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
*   INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
*   EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE AND
*   DOCUMENTATION FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR
*   THE USE OF THE SOFTWARE AND DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL 
*   SUPPORT TO USERS.
*
* HISTORY
*   6/25/2015 Initial Version prepared for NCEI
*
*@*****
**************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "SWATH.h"
#include <sys/stat.h>
#include <unistd.h> 

/*************************************************************/
FILE *openfs();
void read_systim();
void read_parm();
void rama_wnc();

/*************************************************************/
int main(int argc, char *argv[])
{
   FILE        *fp_PARAM, *fp_FNAME;
   char        *string1[] = { "BEGIN_AMSUA", "END_AMSUA"};
   char        *string2[] = { "ORBITAL", "DAILY",
                             "PENTAD",  "MONTHLY" };
   char        input_fname[] = "../input/input.dat";

   char        ama_direct[80], out_direct[80];
   char	       avn_direct[80];
   char        mkcmd[100];
   struct stat st;

/*---------------------------------------------------*
 * Decide 1b input, geo input, and output directory
 *---------------------------------------------------*/
   if (argc > 1)
   {
      strcpy(ama_direct,argv[1]);
      strcpy(avn_direct,argv[2]);
      strcpy(out_direct,argv[3]);
   }
   else
   {
      strcpy(ama_direct,"../temp_fcdr/");
      strcpy(avn_direct,"../temp_avn/");
      strcpy(out_direct,"../temp_output/");
   }

   if (stat(out_direct,&st) != 0)
   {
      sprintf(mkcmd, "mkdir -p %s", out_direct);
      system(mkcmd);
   }

/*---------------------------------------------------*
 * Read in system time
 *---------------------------------------------------*/
   read_systim(string1[0],string2[0]);
   printf("\n AMA2NC/ Successfully read system time !\n"); 
   printf("***************************************************\n");

/*---------------------------------------------------*
 * Open and read parameter file 
 *---------------------------------------------------*/
   fp_PARAM = openfs(input_fname);

   read_parm(fp_PARAM);

/*---------------------------------------------------*
 * Open AMA1B file name file
 *---------------------------------------------------*/
   if((fp_FNAME=fopen("amafile","r")) == NULL)
   {
     printf("AMA2NC/Can't open AMA FCDR filename file: amafile !\n");
     exit(1);
   }
   printf(" Successfully open AMA FCDR filename file: amafile !\n");

/*---------------------------------------------------*
 * Read AMA1B files and generate products to store 
 * in NETCDF4 swath file 
 *---------------------------------------------------*/

   rama_wtcdr(fp_FNAME,ama_direct,avn_direct,out_direct);
   fclose(fp_FNAME);

/*---------------------------------------------------*
 * Read in system time
 *---------------------------------------------------*/

   read_systim(string1[1],string2[0]);

   return 0;
}  /* end of AMA2NC.c */
