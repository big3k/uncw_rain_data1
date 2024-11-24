/**************************************************************************
*@***h* AMSU-A Hydrological TCDR/rama_wtcdr.c
*
* NAME
*   rama_wtcdr.c
*
* PURPOSE
*   Subroutine to read data, generate products and write outputs
*
* DESCRIPTION
*   Program reads AMSU-A FCDR file list, generates
*   products and creates AMSU-A NETCDF4 swath file
*
* Input Files 
*   amafile
*
* Output Files
*   None 
*
* Subroutine Called 
*   gnrt_fname.c, openfs.c, rama_fcdr.c, 
*   gnrt_tcdr.c set_sw.c, avn.c, 
*
* Called by
*   AMA2NC_TCDR.c
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
#include "ESWATH.h"

/*****************************************************************/
void gnrt_fname();
FILE *openfs();
void gnrt_tcdr();
void set_sw();
void avn();
void ini();
size_t rama_fcdr();

/*****************************************************************/
void rama_wtcdr(FILE *fp_FNAME, 
              char ama_direct[], 
              char avn_direct[],
              char out_direct[])
{

   FILE        *fp_AMA1B;

   size_t      numscan;
   short int   i;
   short int   inx_orb = 0;
   short int   hs, he;
   int	       hrint, ret;
   short int   avn_time[10] = { 0, 3, 6, 9, 12, 15, 18, 21, 24, 27};

   char        nc_name[120], bin_name[120], filename[1080], ch[80], fname[80];
   char	       hr[3];
   char        oldsid[]="   ";

/*-----------------------------------------------------*
 * Open AMSU-A 1B orbital files 
 *-----------------------------------------------------*/
   //YDT old while(fgets(fname,FN_length_AMA,fp_FNAME) != NULL)
   while(fgets(fname,80,fp_FNAME)) 
   {
     fname[strlen(fname)-1] = '\0'; //YDT remove training \n
     //YDT fgets(ch,80,fp_FNAME);   /* get rid of the rest of the space */
     printf("\nrama_wnc/AMA file name: %s, FN_length_AMA=%d\n",fname, FN_length_AMA);
     //YDT printf("\nYDT rama_wnc/AMA file name: %s, FN_length_AMA=%d\n",ch, FN_length_AMA);

     strcpy(hdf_fname, fname);
     strcpy(filename, ama_direct);
     strcat(filename, fname);   

     printf("\nYDT hdf_fname: %s\n---",hdf_fname); 
     printf("\nYDT2 filename: %s\n---",filename); 

     numscan = rama_fcdr(filename);

     printf("\nYDT here filename: %s\n",filename); 

/*-----------------------------------------------------*
 * Generate NETCDF4 swath file name
 *-----------------------------------------------------*/
     //strncpy(nc_name, gnrt_fname(), HDF_FNL);
     gnrt_fname(nc_name,out_direct,fname);
     printf("NC4 file name %s, sid is %s\n",nc_name, sid);     

/*-----------------------------------------------------*
 * Determine which AVN files to read and read the data
 *-----------------------------------------------------*/
     hr[2] = '\0';
     hr[0] = fname[35+12];
     hr[1] = fname[36+12];
     hrint = atoi(hr);
     hs = hrint;

     hr[0] = fname[43+12];
     hr[1] = fname[44+12];
     hrint = atoi(hr);
     he = hrint;
     if (he < hs)
        he += 24;

     printf("The starting and ending hours of this orbit are %d and %d \n",hs,he);
     
     for(i = 0; i < 9; i++)
     {
	if(hs >= avn_time[i] && hs < avn_time[i+1]) 
	{
	  avnset[0] = i; 
          avnhr[0] = avn_time[i];
          avnhr[1] = avn_time[i+1];
	  avnset[1] = i+1; 
        }

	if(he >= avn_time[i] && he < avn_time[i+1]) 
	{
	  avnhr[2] = avn_time[i+1]; 
	  avnset[2] = i+1; 
        }
     }

     if(avnset[1] == avnset[2])
       num_avn = 2;
     else if(avnset[1] == avnset[2] - 1)
       num_avn = 3;
     else
       num_avn = -1;

     if(num_avn == 2) 
       printf("This orbit needs 2 AVN files. The times are %d %d \n",avnhr[0],avnhr[1]);
     else if(num_avn == 3)
       printf("This orbit needs 3 AVN files. The times are %d %d %d \n",avnhr[0],avnhr[1],avnhr[2]);

     avn(avn_direct);  

/*-----------------------------------------------------*
 * Calculate from FCDRs and generate TCDR products 
 *-----------------------------------------------------*/
     ini();
     gnrt_tcdr(numscan);
     printf("Finish generating all products \n");
 
/*-----------------------------------------------------*
 * Define and write to the swath file
 *-----------------------------------------------------*/
     printf("rama_wnc_filename: %s\n", nc_name);
     set_sw(nc_name, numscan);

//     printf("rama_wnc/Finish one swath NETCDF4 file !\n");
     printf("*********************************************\n\n");

     inx_orb += 1;
   }

   printf("\n-------------------------------------------------------------\n");
   printf("rama_wnc/    TOTAL OF %d GRANULES ARE PROCESSED!\n",inx_orb);
   printf("-------------------------------------------------------------\n");

}  /* end of rama_wnc.c */
