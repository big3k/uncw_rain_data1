

     integer, parameter :: MAX_NX=200, MAX_NY=200, MAX_NZ=200 !size of temp space for h5 var
     integer, parameter :: MAX_FILES_PER_DAY=3000
     integer, parameter :: DAILY_BINS=14
     ! time bins, in hhmmss format
     
     integer, parameter :: time_bins(DAILY_BINS+1) = [ 000000,014251,032542, &
                  050833,065124,083415,101706,115957,134248, & 
                  152539,170830,185121,203412,221703,235959 ]
     

