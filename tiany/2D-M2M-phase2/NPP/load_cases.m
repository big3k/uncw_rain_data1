%2023/12/19
%result got from /data1/youy/home-directory-umd-deltatb/project/morph_microwave/mfile/
% cal_noaa19_result_step1_data_case_4yudong_image_by_image.m
%  echo case has the following structure: 
%{


                    name: 'event_GCOMW1.mat'
               time_diff: 71.0514
                      cf: [0.1800 0.0900]
                      a2: [1923x1 double]
                      b2: [1923x1 double]
                      c2: [1923x1 double]
                      d2: [1923x1 double]
           gprof_n19_new: [301x301 double]
               gprof_gmi: [301x301 double]
               gprof_n19: [301x301 double]
    gprof_morphed_imager: [301x301 double]
                     lon: [301x301 double]
                     lat: [301x301 double]
              x_dir_grid: 1
              y_dir_grid: 3
                     dir: 1
            gprof_gmi_wz: [301x301 double]
            gprof_n19_wz: [301x301 double]
                 gprof_M: [301x301 double]
              gprof_M_wz: [301x301 double]
%} 


clc
clear
close all;

% %yalei PC
% data_loc='C:\Users\youy\work\project\morph_microwave_grid_gprof\data\noaa19_result_final_step1\';

load_model6d

data_loc='/data1/youy/morph_microwave_grid_gprof/npp_result_final_step1/';

Nian=2014;
Yue=1:12;

%****************************************************
coast=load('coastlines.mat');


jsq=0;
for ijk1=1:length(Nian)
    for ijk2=7 %1:length(Yue)
        NianYue=[num2str(Nian(ijk1)),sprintf('%02d',Yue(ijk2))]

        data_loc1=[data_loc,NianYue,'/'];
        case_list=dir([data_loc1,'case*']);

        if length(case_list)>0
            %****************************************************
            n1=length(case_list);

            %****************************************************
            for i=1:n1

                data_loc2=[data_loc1,case_list(i).name,'/'];
                file_list=dir([data_loc2,'sate_select*.mat']);

                if length(file_list)==1
                    load([data_loc2,file_list(1).name]);

                    gmi=sate_select.gprof_gmi;
                    sounder=sate_select.gprof_n19;
                    imager_moved=sate_select.gprof_morphed_imager; 
                    lon=sate_select.lon;
                    lat=sate_select.lat;

                   % figure out rain system properties
                   if sate_select.name=="event_GCOMW1.mat"
                        sensor=" amsr2"; 
                   else
                        sensor=" ssmis"; 
                   end

                   if sate_select.time_diff < 60
                        diff=" <1hr"; 
                   else 
                        diff=" >=1hr"; 
                   end
        
                   if size(sate_select.a2) < 500
                       ssize = "<500"; 
                   else 
                       ssize = ">=500"; 
                   end
  
                   prop=ssize+diff+sensor; 
                   idx=md_idxes(prop);  
  
                    disp( ['file read: ', data_loc2,file_list(1).name] )
                    disp(['Corr: ', num2str(sate_select.cf), ' Imger name: ', sate_select.name, ...
                          ' time_diff: ', num2str(sate_select.time_diff), ' size: ', num2str(size(sate_select.a2))]) 
                   disp("Prop: " + prop + " model index: " + num2str(idx))

                end
            end
        end
    end
end



