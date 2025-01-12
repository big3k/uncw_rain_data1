

clc
clear
close all;

config_v4  % get all config params 

tic
%***************************************************************

% example: /data1/youy/tropics-hurricane/figure/cat1-cat5-propogate-pmw-beat-IR/2023/FRANKLIN
data_loc1=[CASE_DIR, CASE_YEAR, '/', CASE_NAME, '/']; 

save_loc=[OUTPUT_DIR, CASE_YEAR, '/', CASE_NAME, '/propogated/']; 

% tested
%-- do_step2(data_loc1, save_loc)

%step3, merge at PMW moment
data_loc2=save_loc; 
save_loc=[OUTPUT_DIR, CASE_YEAR, '/', CASE_NAME, '/merged/']; 
%--  do_step3(data_loc1, data_loc2, save_loc)

%step4 
data_loc=save_loc; 
save_loc=[OUTPUT_DIR, CASE_YEAR, '/', CASE_NAME, '/time_sliced/'];
do_step4(data_loc, save_loc);

% 

