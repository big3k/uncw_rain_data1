
%20250105: Original name: ../m2m-merge-version2/
%          step1_image_segment_all_sensors_no_event_combination.m

%2024/07/11
%image segmentation: segment swath into raining events (continous raining areas)

%(1) segment swath
%(2) if two events are close ( < 500 km), we combine two events

%2024/12/12/
%I don't conbine any event, only deal with the large events for now ( > 500 rainning pixels)


clc
clear
close all;

config_v4  % get all config params 

tic
%***************************************************************

% example: '/data1/youy/m2m-merge-version2/daily-20230828/';
yyyymmdd=datestr(datenum(CASE_DAY), 'yyyymmdd'); 
data_loc=[INPUT_DIR, 'daily-', yyyymmdd, '/']; 

% Output dir, only date specific, not case specific
% e.g., OUTPUT_DIR='/data1/tiany/m2m-merge-version4/output/';
% e.g., save_loc='/data1/tiany/m2m-merge-version4/output/daily-20230828/segment-no-combine/';
save_loc=[OUTPUT_DIR, 'daily-', yyyymmdd, '/segment-no-combine/'];
mkdir (save_loc); 

% events segmentation 
do_step1(data_loc, save_loc) % 

% pick event by lat/lon region, case_specific
data_loc=save_loc;  % input from setp 1 output
save_loc=[OUTPUT_DIR, CASE_NAME, '/daily-', yyyymmdd, '/segment-no-combine/'];  
% e.g.., /data1/tiany/m2m-merge-version4/output/Franklin/daily-20230828/segment-no-combine/
mkdir (save_loc); 
pick_event_by_latlon(data_loc, save_loc, ...
                              CASE_DAY, CASE_LON1, CASE_LON2, CASE_LAT1, CASE_LAT2)

%step2: use each image as an anchor, propogate all possible image (in a +/- 4 hour range) to this
%image, so later on, we can create a complete image as much as possible in
%space

data_loc1=save_loc; 
save_loc=[OUTPUT_DIR, CASE_NAME, '/daily-', yyyymmdd, '/propogated-to-this-momen/']; 
mkdir (save_loc); 
do_step2(data_loc1, save_loc)

%step3, merge at PMW moment
data_loc2=save_loc; 
save_loc=[[OUTPUT_DIR, CASE_NAME, '/daily-', yyyymmdd, '/merged-at-this-moment/']; 
mkdir (save_loc); 
do_step3(data_loc1, data_loc2, save_loc)

% 

