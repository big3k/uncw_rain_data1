/***************************************************************************
 *  Program Name      : minmax.c
 *  Type              : Subroutine
 *  Function          : Program calculates min and max of an array 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : set_sw.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   7/26/2011      v1.0
 *************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "ESWATH.h"

/*********************************************************/
float *minmax(float array[][NUMSPOT_A], int nx, int ny)
{

   int ix, iy;
   float mm[2], *pa;
   float min, max;

   min = array[0][0];
   max = array[0][0];
   
//   printf("ini min max are %f %f\n", min, max);
   for(ix=0; ix<nx; ++ix)
   {
       for(iy=0; iy<ny; ++iy)
       {
          if(min > array[ix][iy]) min = array[ix][iy];
          if(max < array[ix][iy]) max = array[ix][iy];
       }
   }

   mm[0] = min;
   mm[1] = max;
//   printf("res min max are %f %f\n", min, max);

   pa = &mm[0];
    
   return pa;

}  /* end of minmax.c */
