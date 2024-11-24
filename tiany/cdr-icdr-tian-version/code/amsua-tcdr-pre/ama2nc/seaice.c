/***************************************************************************
 *  Program Name      : seaice.c
 *  Type              : Subroutine
 *  Function          : Program calculates sea ice concentration  
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : gnrt_tcdr.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   4/25/2001      v2.0
 *************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************************/
//float  corr();

/*******************************************************************/
void seaice(short int iscan)
{
    char	 lstag;

    short int    ifov;

    float        tb23, tb31, tb50;
    float        alza, u, fice, alat;
    float        eps, eps_water, eps_ice;
    float        a, b, c, d;
    float	 difftb;

/*--------------------------------------------------------*
 * Sea ice computation for ocean FOVs 
 *--------------------------------------------------------*/
    for (ifov = 2; ifov < NUMSPOT_A - 2; ifov++) /* exclude orbital edge FOVs */
    {
      lstag = stype_a2[iscan][ifov];

      if (lstag == 0 && fcdr_23[iscan][ifov] > 0.)
      { 
          alat = lat_a2[iscan][ifov]; 
          alza = lza_a2[iscan][ifov];
          u = cosf(alza * PI/180.0);
          eps_water = 0.1824 + 0.9048 * u - 0.6221 * u * u;

/*--------------------------------------------------------*
 * Perform AT test
 *--------------------------------------------------------*/

          tb23 = fcdr_23[iscan][ifov];
          tb31 = fcdr_31[iscan][ifov];
          tb50 = fcdr_50[iscan][ifov];

          a = 1.84 - 0.723 * u;
  	  b = -0.00088;
	  c = 0.0066 + 0.0029 * u;
	  d = -0.00926;
          eps = a + b*tb23 + c*tb31 + d*tb50;

          if(alat < 50.0 &&  alat > -50.0)  
             sice[iscan][ifov] = 0.0;  
          else
          {
             difftb = tb23 - tb31;
	     if(difftb < 5.)
	       eps_ice = 0.93;
	     else if(difftb >= 5. && difftb <= 10.)
               eps_ice = 0.87;
	     else 
	       eps_ice = 0.83;

             fice = 100 * (eps - eps_water)/(eps_ice - eps_water);

             if(fice < 30.0 ) /* cutoff value of 30% is applied */
	        fice = 0; 
             if(fice > limit_A.SIce_upper) 
                fice = limit_A.SIce_upper;
             if(fice < limit_A.SIce_lower) 
                fice = BELOW_PROD;

             if(fice > 0.)
               sice[iscan][ifov] = fice * SICE_SCAL;
             else
	       sice[iscan][ifov] = fice;

          }   /* end of eps check */

       } 

       else if(fcdr_23[iscan][ifov] > 0.)  /* land and coast */
          sice[iscan][ifov] = INDETERM;

    }   /* end of ifov loop */

} /* end of seaice.c */    
