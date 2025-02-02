

function plot_case(CASE_YEAR, CASE_NAME) 

% example input: 
%------------------------------
% CASE_YEAR='2023';
% CASE_NAME='FRANKLIN';

config_v4  % get all config params 

%***************************************************************

% example: /data1/youy/tropics-hurricane/figure/cat1-cat5-propogate-pmw-beat-IR/2023/FRANKLIN
data_loc1=[CASE_DIR, CASE_YEAR, '/', CASE_NAME, '/']; 

save_loc=[OUTPUT_DIR, CASE_YEAR, '/', CASE_NAME, '/propogated/']; 

% tested
%do_step2(data_loc1, save_loc)

%step3, merge at PMW moment
%data_loc2=save_loc; 
%save_loc=[OUTPUT_DIR, CASE_YEAR, '/', CASE_NAME, '/merged/']; 
%do_step3(data_loc1, data_loc2, save_loc)

%step4 
%data_loc=save_loc; 
save_loc=[OUTPUT_DIR, CASE_YEAR, '/', CASE_NAME, '/time_sliced/'];
plot_step4(save_loc);

%step5
%data_loc=save_loc; 
save_loc=[OUTPUT_DIR, CASE_YEAR, '/', CASE_NAME, '/time_interpolated/'];
plot_step5(save_loc);

% 

end % function 

