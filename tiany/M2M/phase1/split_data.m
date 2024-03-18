% YDT 20230714
% Separate data into two sets:  training and validation 
% training: cases for 2014-2017 (867 cases) 
% validation: cases for 2018-2020 (599 cases) 

clc
clear
close('all');

data_loc='deltatb/deltatb_ocean_2020/noaa19_result_final_step1_bk/';

%define training and validation by time periods. also need to change the last 4 lines correspondingly 
Nian=2014:2017;   % for training
outf='training.mat'; 
%Nian=2018:2020;   % for validation
%outf='validation.mat'; 

Yue=1:12;

%****************************************************
gmi=[]; %gmi as reference data, in sate_select.a2
n19=[]; %N19 original  as input 1, in sate_select.b2
half_half=[]; % simple mean b/w n19 and imager_moved: 0.5*N19+0.5*imager_moved, as Yalei's original experiment
imager_moved=[]; % proporaged imager as input 2, in sate_select.d2

time_diff_pixel=[]; %time difference between propoaged imager and N19
imager_name=[];   %AMSR2 or SSMISs
sys_size=[]; % precipitation system size, definded by raining pixels

%corr. between n19 and imagers. small means precip. system evolvs a lot
%do not use this kind of cases ???, or use as an adjust variable
cf_imager_n19=[]; 

% each case "sate_select" is the following structure: 
% sate_select =
% 
%  struct with fields:
%
%             name: 'event_F17.mat'
%        time_diff: 8.8840
%               cf: [0.4400 0.7100]
%               a2: [325x1 double]
%               b2: [325x1 double]
%               c2: [325x1 double]
%               d2: [325x1 double]
%    gprof_n19_new: [301x301 double]
% 
% for cf[]: the first value is between GMI and original N19 (i.e, corr(sate_select.a2,sate_select.b2)). 
% The second value is between GMI and the adjusted N19 (i.e., corr(sate_select.a2,sate_select.c2)).


jsq=0;
for ijk1=1:length(Nian)
    for ijk2=1:length(Yue)
        NianYue=[num2str(Nian(ijk1)),sprintf('%02d',Yue(ijk2))]

        data_loc1=[data_loc,NianYue,'/'];
        case_list=dir([data_loc1,'case*']);

        if length(case_list)>0
            %****************************************************
            n1=length(case_list);
            %****************************************************
            for i=1:n1

	        jsq=jsq+1;
	        fprintf("Total cases: %d\n", jsq); 

                %YDT data_loc2=[data_loc1,case_list(i).name,'\'];
                data_loc2=[data_loc1,case_list(i).name,'/'];
                file_list=dir([data_loc2,'sate_select*.mat']);

                if length(file_list)==1
                    load([data_loc2,file_list(1).name]);

		    % construct table columns
		    n_rain=length(sate_select.a2);

		    %gmi, to be used as the reference for the predicted 
		    gmi=[gmi;sate_select.a2];

		    %sys_size
		    tmp_size=ones(n_rain, 1)*n_rain; 
		    sys_size=[sys_size;tmp_size]; 

		    % n19, propagated imager, and the "0.5/0.5" scheme
		    n19=[n19;sate_select.b2];
		    imager_moved=[imager_moved;sate_select.d2];
		    half_half=[half_half;sate_select.c2];

		    % n19, propagated imager, and the "0.5/0.5" scheme
	            tp2=ones(n_rain, 1)*sate_select.time_diff;
                    time_diff_pixel=[time_diff_pixel;tp2];

		    % corr b/w n19's and propagated imager's log(rain) 
		    tp6=ones(n_rain, 1)*sate_select.cf(1, 1); 
                    cf_imager_n19=[cf_imager_n19;tp6];

		    % name of the imager as string vector 
		    tmp=cell(n_rain, 1);
		    tmp(:)={string(sate_select.name)}; 
		    % convert cell array to string array
		    imager_name=[imager_name;string(tmp)]; 
                end
            end

        end
    end
end

training_data=table(n19, imager_moved, time_diff_pixel, imager_name, cf_imager_n19, half_half, gmi); 
save(outf, "training_data")
%validation_data=table(n19, imager_moved, time_diff_pixel, imager_name, cf_imager_n19, half_half, gmi); 
%save(outf, "validation_data")

