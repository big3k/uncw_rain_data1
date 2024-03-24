% 20240323 
% Run all NPP cases, with each case corrected with M2M 
%  echo input case has the following structure: 
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

data_loc='/data1/youy/morph_microwave_grid_gprof/npp_result_final_step1/';

Nian=2014:2023;
Yue=1:12;

%****************************************************

all_results=table; 

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

                data_loc2=[data_loc1,case_list(i).name,'/'];
                file_list=dir([data_loc2,'sate_select*.mat']);

                if length(file_list)==1
                    case_data=strcat(data_loc2, file_list(1).name) 
                    result = m2m_by_case(case_data); 
                    if ~ismissing(result)  % only save 
                      all_results = [all_results; result]; 
                     end
                    close all; 
               end
            end
        end
    end
end

all_results = renamevars(all_results, ["Var1", "Var2", "Var3", "Var4"], ["case_path", "mat_name", "Perf_metrics", "Sys_prop"]); 

save("all_results.mat", "all_results"); 







