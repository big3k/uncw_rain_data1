/***************************************************************************   
 *  Program Name      : openfs.c     
 *  Type              : Subroutine
 *  Function          : Program opens file with the given file name 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : AMA2NC_TCDR.c, rama_wtcdr.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "ESWATH.h"

/******************************************************************/
FILE *openfs(char *fname)
{
   FILE     *fp;

/*----------------------------------------------------------*
 * Open input file 
 *----------------------------------------------------------*/
   fp = fopen(fname, "r");

   if(fp == NULL)
   {
     printf("openfs/Cannot open input file %s\n", fname); 
//     exit(0);
   }
   else
     printf("openfs/Successfully open input file %s\n", fname); 

   return fp;
         
} /* end of openfs.c */                             
