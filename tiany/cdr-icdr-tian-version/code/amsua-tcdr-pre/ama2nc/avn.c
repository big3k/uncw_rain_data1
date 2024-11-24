/***************************************************************************
 *  Program Name      : avn.c
 *  Type              : Subroutine
 *  Function          : Program reads AVN data  
 *  Input Files       : AVN binary data files
 *  Output Files      : None
 *  Subroutine Called : read_avn.c 
 *  Called by         : rama_wtcdr.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *    3/2/2001      v2.0
 *************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
void read_avn();

/*****************************************************************/
void avn(char avn_direct[])
{

   if(num_avn < 0) 
   {
     printf("Warning: Timing of the 1B* data is out of range!\n");
     printf("TPW and CLW fields may not be generated!\n");
     return;
   }

/*------------------------------------------------*
  Read the first set of AVN data 
 *------------------------------------------------*/
   printf("Read the first set of AVN data\n"); 
   read_avn(0,avn_direct); 
   
/*------------------------------------------------*
  Read the second set of AVN data 
 *------------------------------------------------*/
   printf("Read the second set of AVN data\n"); 
   read_avn(1,avn_direct); 
   
/*------------------------------------------------*
  Read the third set of AVN data if needed
 *------------------------------------------------*/
   if(num_avn == 3)
   {
     printf("Read the third set of AVN data\n"); 
     read_avn(2,avn_direct); 
   }
   
} /* end of avn.c */
