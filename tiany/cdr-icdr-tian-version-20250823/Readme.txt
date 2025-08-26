8/26/2025: 
 wrote ./code/overwrite_fcdr/change_data.c to overwrite fcdr's four TB values with ones from Yalei's
 matlab dump. 


8/25/2025: 
Reset all numbers in ./code/amsua-fcdr-pre/input/mu_dr_k.dat to zero, effectively turning off the original 
calibration method. 

8/23/2025:

./code/amsua-fcdr-pre/input/mu_dr_k.dat is read in by ./code/amsua-fcdr-pre/ama2nc/read_itdata.c

then the calibaration is done in: 

./code/amsua-fcdr-pre/ama2nc/getcal.c

whicn can be easily turned off. 

