/**************************************************************************
*@***h* AMSU-A Hydrological TCDR/gnrt_tcdr.c
*
* NAME
*   gnrt_tcdr.c
*
* PURPOSE
*   Subroutine to generate products
*
* DESCRIPTION
*   Program generate a suite of level 2 products
*
* Input Files 
*   None
*
* Output Files
*   None 
*
* Subroutine Called 
*   seaice.c, tpwclw.c, stemp.c, emi.c
*
* Called by
*   rama_tcdr.c
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
void seaice();
void tpwclw();
void stemp();
void emi();

/*****************************************************************/
void gnrt_tcdr(short int numscan)
{

   short int   		iscan, ichan;

   unsigned int 	qa_ind;
   unsigned char        qa_time, qa_cal, qa_eloc;
   unsigned short       qa_cal_chan;
   unsigned char        a1, a2, a;

/*---------------------------------------------------------------*
 * Check if the scanline is good
 *---------------------------------------------------------------*/
   //printf("numscan: %d\n",numscan);

   for (iscan = 0; iscan < numscan; iscan++)
   {
       //qa_ind = qa_prod[iscan][0] & 1<<7;
       qa_ind = 0;
       for (ichan = 0; ichan < NUMCHAN_A; ichan++)
           qa_ind = qa_ind || (qa_prod[iscan][ichan] & 1<<7);
       // printf("Quality indictor: %ld\n",qa_ind);

/*---------------------------------------------------------------*
 * Toss off the scanlines that are bad and flag them as 
 * "do_not_use_scan". 
 *--------------------------------------------------------------*/
       if (qa_ind == 0)   // good scanline 
       {
           //printf("generating products at line %d.\n", iscan);
           seaice(iscan);   
           tpwclw(iscan);
           stemp(iscan);
           emi(iscan);
       }  

    }   /* end of iscan loop */

}  /* end of gnrt_prod.c  */
