/***************************************************************************
 *  Program Name      : tpwclw.c
 *  Type              : Subroutine
 *  Function          : Program calculates total precipitable water (TPW) 
 *			and cloud liquid water (CLW) (ocean only)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : vapor_liquid(), emissivity_water(), epsilon() 
 *  Called by         : gnrt_tcdr.c
 *
 *************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "ESWATH.h"

/**************************************************************/
double      ao = 1.9900e-1;
double      bo = -1.1702e-3;
double      co = 1.8762e-6;

double      av = 5.2099e-3;
double      bv = 1.5259e-6;

double      al = 1.1820e-1; 
double      bl = -3.4876e-3;
double      cl = 5.0130e-5;

double      au = 4.8232E+1;
double      bu = 7.8745e-1;
double      cu = 3.1031e-2;

double      ad = 4.4381e+1;
double      bd = 8.0172e-1;
double      cd = 7.7450e-2;

/**************************************************************/
void vapor_liquid(float tb[], float wind, float tsavn, float tl, float theta, float mu, short int product[], short int iscan, short int ifov, short int below_prod);

void emissivity_water(float s0, float s1, float s2, float s3, float s4, float s5[]);

void epsilon(float s0, float s1, float s2, double *s3, double *s4);

//float corr(); 

/**************************************************************/
void tpwclw(short int iscan)
{
    char lstag;

    short int i, ii, ifov, ilat, ilon;     
    int         year, mon, dd;
    int         hh, mm, ss; 
    int 	indx, d0;
    short int product[2];
    short int below_prod;

    float tb23, tb31, tb50, tb[2], tbir, theta, mu;
    float wind, wind_u, wind_v, tsavn, ts_2m_ncep, v; 
    float df1, tl;
    float alat, alon, aice;
    float interpt;
    char  buffer[80]; 
/*
  product[0]  water vapor
  product[1]  liquid
*/

    if(num_avn < 0)
      return;
        
    // calculate interp   
    sscanf(str_scantime[0], "%4d-%2d-%2dT%2d:%2d:%2d",
        &year, &mon, &d0, &hh, &mm, &ss);
    sscanf(str_scantime[iscan], "%4d-%2d-%2dT%2d:%2d:%2d",
        &year, &mon, &dd, &hh, &mm, &ss);
//    printf("dd=%d hh=%d min=%d \n", dd,hh,mm);
//    hr = geo_scantime[iscan].hour;
//    min = geo_scantime[iscan].minute;
//    if (geo_scantime[iscan].doy > geo_scantime[0].doy)
    if (dd != d0)
       hh += 24;

    indx = -1;
    if(num_avn == 2)
    {
      ii = 0;
      indx = 0; 
    }
    else if(num_avn == 3)
    { 
        for(i = 0; i < 2; i++)
        {
           if(hh >= avnhr[i] && hh < avnhr[i+1]) 
	   {
	     ii = i;
	     indx = 0;
	   }
        }
    }

    if(indx == -1) 
    {
      printf("tpwclw/Point %d %d doesn't have corresponding AVN data\n",iscan,ifov);
      printf("%d %d %d %d %d\n",hh,mm,avnhr[0],avnhr[1],avnhr[2]);
      return;
    }

          //if(avnhr[ii] == 24 && hr < 3)
          //  interpt = (hr + min/60.)/3.;
          //else
    interpt = (hh - avnhr[ii] + mm/60.)/3.;

    // process each fov
    for (ifov = 0; ifov < NUMSPOT_A; ifov++)
    { 
      product[0] = MISSING;
      product[1] = MISSING;

      below_prod = BELOW_PROD;

      if(fcdr_23[iscan][ifov] != MISSING && fcdr_31[iscan][ifov] != MISSING)
      {
        lstag = stype_a2[iscan][ifov]; 
      
        if(lstag == 0)    // ocean 
        {
          tb23 = fcdr_23[iscan][ifov];
          tb31 = fcdr_31[iscan][ifov];
          tb50 = fcdr_50[iscan][ifov];

          tb[0] = tb23;
          tb[1] = tb31;

          alat = lat_a2[iscan][ifov];
          alon = lon_a2[iscan][ifov];

          aice = sice[iscan][ifov];
          theta = lza_a2[iscan][ifov]; 
          theta = PI * theta / 180.0;
          mu = cos(theta);

          ilat = 90.0 - alat + 0.5;
          ilon = 180.0 + alon + 0.5;
     	  if(ilat < 0)
            ilat = 0;
      	  if(ilat > NUMROW_AVN - 1)
            ilat = NUMROW_AVN - 1;
          if(ilon < 0)
            ilon = 0;
      	  if(ilon > NUMCOL_AVN - 1)
            ilon = NUMCOL_AVN - 1;


          wind_u = windu_avn[ii][ilat][ilon] + (windu_avn[ii+1][ilat][ilon]-windu_avn[ii][ilat][ilon])*interpt;
          wind_v = windv_avn[ii][ilat][ilon] + (windv_avn[ii+1][ilat][ilon]-windv_avn[ii][ilat][ilon])*interpt;
          tsavn = ts_avn[ii][ilat][ilon] + (ts_avn[ii+1][ilat][ilon]-ts_avn[ii][ilat][ilon])*interpt;
          ts_2m_ncep = ts2m_avn[ii][ilat][ilon] + (ts2m_avn[ii+1][ilat][ilon]-ts2m_avn[ii][ilat][ilon])*interpt;
          v = tpw_avn[ii][ilat][ilon] + (tpw_avn[ii+1][ilat][ilon]-tpw_avn[ii][ilat][ilon])*interpt;

          wind = sqrt(wind_u*wind_u + wind_v*wind_v);

          tbir = 285; // use constant because AVHRR data is not available  
          tl = tbir - 273.15;
          if(tl < 0.0 ) 
            tl =5;

          if(aice > PECENT_ICE)
	  {
            product[0] = INDETERM_SICE;
            product[1] = INDETERM_SICE;
	  }
          else 
	  {
            df1 = 2.85 + 0.020 * tb23 - 0.028 * tb50;

            if((alat > 50.0 ||  alat < -50.0) && df1 > 0.2)
            {
              product[0] = INDETERM_SICE;
              product[1] = INDETERM_SICE;
            }
            else
	    {
              vapor_liquid(tb, wind, tsavn, tl, theta, mu, product, iscan, ifov, below_prod);

	      if(product[0] > limit_A.TPW_upper * TPW_SCAL) 
		product[0] = limit_A.TPW_upper * TPW_SCAL;
	      if(product[0] < limit_A.TPW_lower * TPW_SCAL) 
		product[0] = BELOW_PROD;

	      if(product[1] > limit_A.CLW_upper * CLW_SCAL) 
		product[1] = limit_A.CLW_upper * CLW_SCAL;
	      if(product[1] < limit_A.CLW_lower * CLW_SCAL) 
                product[1] = BELOW_PROD;
            }
          }
        }  

        else   // land and coast 
        {
          product[0] = INDETERM; 
          product[1] = INDETERM; 

        } // end of land algorithm 
       
      } // end of non-missing data processing 

      // return retrieval values to the output 
      tpw[iscan][ifov] = product[0];
      clw[iscan][ifov] = product[1];

    }  // end of loop ifov 

} /* end of tpwclw.c */


/**************************************************************/
void vapor_liquid(float tb[], float wind, float tsavn, float tl, float theta, float mu, short int product[], short int iscan,short int ifov, short int below_prod) 
{
/*   #define  kv23  4.80423e-3 * 1.14 / 1.05 */
   #define  kv23  4.80423e-3 
   #define  kv31  1.93241e-3  
   #define  a1  kv31/kv23
   #define  salinity  35.5
   #define  earthrad  6371
   #define  satheight  833
   #define  coeA       0.968
   #define  coeB      -1.878

   #define  coe1_a -13.498
   #define  coe1_b 12.557
   #define  coe1_c -1.772512719
   #define  coe1_d -2.349452392
   #define  coe1_e 1.118339404
   #define  coe1_f 3.665828283

   #define  coe2_a 18.06
   #define  coe2_b -28.616
   #define  coe2_c 9.949884622
   #define  coe2_d -1.86678768
   #define  coe2_e 1.089603296
   #define  coe2_f 2.73654427

   #define  coe3_a 39.174
   #define  coe3_b -78.037
   #define  coe3_c 34.08133358
   #define  coe3_d 4.907517964
   #define  coe3_e 0.990351243
   #define  coe3_f -18.86487998

   float  em23hv[2], em31hv[2], tauo23, tauo31;
   float  sinthetas, costhetas, tb23, tb31;
   float  em23, em31, a0, a2, b0, b1, b2, kl23, kl31;
   float  tpwtmp, clwtmp, tmp;

   tb23 = tb[0];
   tb31 = tb[1];
  
   sinthetas = sin(theta)* earthrad/(earthrad + satheight);
   sinthetas = pow(sinthetas, 2);
   costhetas = 1.0 - sinthetas;

   emissivity_water(wind, theta, tsavn, salinity, 23.8e9, em23hv);
   em23 = costhetas*em23hv[1] + sinthetas*em23hv[0];
         
   emissivity_water(wind, theta, tsavn, salinity, 31.4e9, em31hv);
   em31 = costhetas*em31hv[1] + sinthetas*em31hv[0];
		 
   tauo23 = 3.21410e-2 - 6.31860e-5*tsavn;
   tauo31 = 5.34214e-2 - 1.04835e-4*tsavn;
		 
   kl23 = 1.18203e-1 - 3.48761e-3*tl + 5.01301e-5*tl*tl;
   kl31 = 1.98774e-1 - 5.45692e-3*tl + 7.18339e-5*tl*tl;

   b0 = .5*kl23/(kv23*kl31 - kv31*kl23);
   b1 =  kl31/kl23;
   b2 = - 2.0*(tauo31 - b1*tauo23)/mu + (1.0 - b1)*log(tsavn) + 
               log(1.0 - em31) - b1*log(1.0 - em23);
    
   a0 = -.5*kv23/(kv23*kl31 - kv31*kl23);
   a2 = -2.0*(tauo31 -a1*tauo23)/mu + (1.0 - a1)*log(tsavn) + 
              log(1.0 - em31) - a1*log(1.0 - em23);
         
   tpwtmp = coeA * (mu*b0*(log(tsavn - tb31) - b1*log(tsavn - tb23) - b2)) + coeB; 
   clwtmp = mu*a0*(log(tsavn - tb31) - a1*log(tsavn - tb23) - a2); 

   if (clwtmp < 0.0)  
    clwtmp = 0.;  

/* Move TPW lower limit check from main routine to here */ 
   if(tpwtmp < 0) 
     product[0] = below_prod;

/* Implement angular correction */
   else
   {
     if(clwtmp >= 0. && clwtmp < 0.2)
     {
        tmp = tpwtmp + coe1_a*mu*mu+ coe1_b*mu + coe1_c;
        if(tmp > 0.)
          tpwtmp = coe1_d*log(tmp) + coe1_e*tmp + coe1_f;
     }
     else if (clwtmp >= 0.2 && clwtmp < 0.8)
     {
        tmp = tpwtmp + coe2_a*mu*mu+ coe2_b*mu + coe2_c;
        if(tmp > 0.)
          tpwtmp = coe2_d*log(tmp) + coe2_e*tmp + coe2_f;
     }
     else if(clwtmp >= 0.8)
     {
        tmp = tpwtmp + coe3_a*mu*mu+ coe3_b*mu + coe3_c;

        if(tmp > 0.)
        {
// 5/1/06: flag TPW by its negative value if CLW >= 0.8 mm because of possible rain contamination
          tpwtmp = -(coe3_d*log(tmp) + coe3_e*tmp + coe3_f);

// 5/1/06: Adjust certain values to avoid confusion with existing flags 
          if(fabs(tpwtmp) == 9.9) // flag for missing data: -99
            tpwtmp = -10.;
          else if(fabs(tpwtmp) == 1.0) // flag for land or coast: -10
            tpwtmp = -1.1;
          else if(fabs(tpwtmp) == 0.8) // flag for sea ice: -8
            tpwtmp = -0.9;
          else if(fabs(tpwtmp) == 0.2) // flag for BELOW_PROD: -2
            tpwtmp = -0.3;
        }

     }

     product[0] = TPW_SCAL * tpwtmp;
     product[1] = CLW_SCAL * clwtmp;

   }

}

/**************************************************************/
void emissivity_water(float wind, float angle, float t, float s, float f, float em_vector[2])
{
/*
;     Variables: angle = incident angle in radians (input)
;         t     = temperature (K)
;         s     = sea water salinity (per thousand)
;         f     = frequency (Hz)
;         wind  = wind speed (m/s)
;
;     Internal Variables
;
;         foam  = foam fraction
;         g,tr  = emperical functions for wind induced
;                 changes in reflection coefficient
;
;     Output
;         Emissivity vector (eh, ev)
;
;     Literature Sources:
;
;     (1) Calm water emissvity
;         Klein and Swift (KS) emissivity for calm water (1977) IEEE Trans.
;         Antennas Propag., 25, 104-111
;     (2) Roughtness correction part for emissivity
;         Francies et al. 1983
;         Satellite microwave remote sensing
;         Observations was made by Hollinger (1971), HO71
;     (3) Foam emissivity is based on
;         Stogryn (1972). JGR, 77, 641-653, ST72
;------------------------------------------------------------------- */
  
  float  g, tr, ref, rh, rv, rclear, rfoam;
  float  foam,  degre,  degre2,  degre3,  degre10, xx, yy, rr, dd; 
  double tmp;

  struct complx {
        double x;
        double y;
  }; 
  struct complx eps = {0.0, 0.0};
  struct complx aid1 = {0.0, 0.0};
  struct complx aid2 = {0.0, 0.0};
  struct complx aid3 = {0.0, 0.0};
  struct complx aid = {0.0, 0.0};
 
  em_vector[0]= 1.0;
  em_vector[1]= 1.0;

  epsilon(t, s, f, &eps.x, &eps.y); 

  /* eps is the complex dielectric constant of saltine water */
  degre = angle / PI * 180.0;
  xx = eps.x - pow(sin(angle),2);
  yy = eps.y; 
  dd = atan2(yy, xx)/2.0; 
  rr = pow( (pow(xx,2) + pow(yy,2)), 0.25);
  aid1.x = rr*cos(dd);
  aid1.y = rr*sin(dd);

  /* Fractional amount of foam coverage */
  foam = 7.751e-6 * pow(wind, .231);
  if (foam < 0.0) foam = 0.0;
  if (foam > 1.0) foam = 1.0;

  /* Compute the emissivity for horizontal polarization */
  aid2.x = cos(angle) - aid1.x;
  aid2.y = - aid1.y;

  aid3.x = cos(angle) + aid1.x;
  aid3.y = aid1.y;

  degre2 = pow(degre, 2);
  degre3 = pow(degre, 3);
  degre10 = pow(degre, 10);

  /* Emperical functions for wind induced reflection change */

  g = 1.0 - 1.748e-3 * degre - 7.336e-5 * degre2 + 1.044e-7 *degre3;
  tr = wind * (1.15e-1 + 3.8e-5 * degre2 ) * sqrt(f*1.0e-9);
  aid.x = (aid2.x * aid3.x + aid2.y * aid3.y)/(pow(aid3.x, 2) + pow(aid3.y,2));
  aid.y = (aid2.y * aid3.x - aid2.x * aid3.y)/(pow(aid3.x, 2) + pow(aid3.y,2));
  tmp = sqrt(aid.x * aid.x + aid.y * aid.y); 
  ref = pow( tmp,2);
  rclear = ref - tr / t;

 /* Reflection coeff. of foam covered sea surface */
  rfoam = 1.0 - (208.0 + 1.29e-9 * f)/ t*g;

 /* Linear interpolation between foam free and foam covered reflection coeff.*/
  rh = ( 1.0 - foam) * rclear + foam * rfoam;
  if (rh > 1.0) rh = 1.0;
  if (rh < 0.0) rh = 0.0;
  em_vector[0] = 1.0 - rh;

  /* Compute the emissivity for vertical polarization */
  aid2.x = eps.x * cos(angle) - aid1.x;
  aid2.y = eps.y * cos(angle) - aid1.y;
  aid3.x = eps.x * cos(angle) + aid1.x;
  aid3.y = eps.y * cos(angle) + aid1.y;

  /* Emperical functions for wind induced reflection changes */
  g  = 1.0 - 9.946e-4 * degre + 3.218e-5 * degre2 - 
       1.187e-6 * degre3 + 7.e-20*degre10;
  tr = wind*(1.17e-1-2.09e-3*exp(7.32e-2*degre)) *sqrt(f*1.0e-9);

  aid.x = (aid2.x * aid3.x + aid2.y * aid3.y)/(pow(aid3.x, 2) + pow(aid3.y,2));
  aid.y = (aid2.y * aid3.x - aid2.x * aid3.y)/(pow(aid3.x, 2) + pow(aid3.y,2));

  tmp = sqrt(aid.x * aid.x + aid.y * aid.y); 
  ref = pow( tmp,2);
  rclear = ref - tr / t;

 /*  Reflection coeff. of foam covered sea surface */
  rfoam = 1.0 - (208.0 + 1.29e-9 * f)/ t*g;

 /* Linear interpolation between foam free and foam covered reflection coeff.*/
  rv = ( 1.0 - foam) * rclear + foam * rfoam;
  if (rv > 1.0) rv = 1.0;
  if (rv < 0.0) rv = 0.0;
  em_vector[1] = 1.0 - rv;

}


/**************************************************************/
void epsilon(float t1, float s, float f,  double *i, double *j)
{
/*

; this function calculates the dialectric constant of saline water
;
; Reference:
;
; Microwave remote sensing by Ulaby et al (1984)  pp 2022

; T1    Water Skin temperature (K)
; S     Salinity  Parts per thousand
; F     Frequency (Hz)

 */

  float eo, esw, eswo, eswi, a, tswo, tsw, b, sswo, d, fi, ssw;
  float t, t2, t3, s2, s3, epsp, epspp;	  

  t    = t1 - 273.0;
  t2 = t*t;
  t3 = pow(t, 3);
  s2 = s*s;
  s3 = pow(s, 3);
  eswi = 4.9;
  eo   = 8.854e-12;
  eswo = 87.134 - 1.949e-1 * t - 1.276e-2 * t2 + 2.491e-4 * t3;

  a = 1.0 + 1.613e-5 * t*s - 3.656e-3 * s + 3.210e-5 * s2 - 4.232e-7 * s3;
  esw = eswo * a;
  tswo = 1.1109e-10 - 3.824e-12 * t + 6.938e-14 * t2 - 5.096e-16 * t3;
  b = 1.0 + 2.282e-5 * t * s - 7.638e-4 * s - 7.760e-6 * s2 + 1.105e-8 * s3;
  tsw = tswo*b;
  epsp = eswi + (esw - eswi)/ (1.0 + pow( f * tsw, 2) );
  sswo = s*(0.18252-1.4619e-3*s+2.093e-5*s2-1.282e-7*s3);
  d  = 25.0 - t;
  fi = d * (2.033e-2 + 1.266e-4 * d + 2.464e-6 * d * d -   
       s * (1.849e-5 - 2.551e-7 * d + 2.551e-8 * d * d)); 
  ssw = sswo * exp(-fi);
  epspp = tsw * f * (esw - eswi) / (1.0 + pow (tsw * f, 2) );
  epspp = epspp + ssw/(2.0 * PI * eo * f);
  *i =   epsp;
  *j = - epspp;

}


