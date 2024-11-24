/***************************************************************************
 *  Program Name      : ini.c
 *  Type              : Subroutine
 *  Function          : Program initiate the variables to INDETERM
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : rama_wtcdr.c
 *
 *************************************************************************/
#include "AMA2NC_INCLUDE.h"
#include "ESWATH.h"

/**************************************************************/
void ini()
{
    int         iscan, ispot;

    for (iscan = 0; iscan < MAXSCANLINE_A; iscan ++) {
        for (ispot = 0; ispot < NUMSPOT_A; ispot ++) {
            clw[iscan][ispot] = INDETERM;
            tpw[iscan][ispot] = INDETERM;
            sice[iscan][ispot] = INDETERM;
            ts[iscan][ispot] = INDETERM;
            em1[iscan][ispot] = INDETERM;
            em2[iscan][ispot] = INDETERM;
            em3[iscan][ispot] = INDETERM;
        }
    }
}


