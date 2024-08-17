%08/12/2024
%for each case, attach the nearest MRMS rain rates

clc
clear
close('all');

data_loc='/data1/youy/grid_satellites/noaa20-gprof-events-imager-propogated-final-land/';

%MRMS: 2020/11 to current
%Satellite: 2017/11 to 2023/05

Nian=2021:2023;
Yue=1:12;
%****************************************************
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

                disp([mfilename,'.m ', NianYue,' ', num2str(i),' of ', num2str(n1)]);
                data_loc2=[data_loc1,case_list(i).name,'/'];
                file_list=dir([data_loc2,'sate_select*.mat']);

                if length(file_list)==1
                    casef=[data_loc2,file_list(1).name]; 
                    %disp(data_loc2) 
                    %disp(casef) 
                    % sample output
                    %/data1/youy/grid_satellites/noaa20-gprof-events-imager-propogated-final-land/202104/case_0418/
                    %/data1/youy/grid_satellites/noaa20-gprof-events-imager-propogated-final-land/202104/case_0418/sate_select_case_229.mat
                    % check if there is overlap, and save matched data 
                    upscale_subset(data_loc2, casef); 

                end
            end
        end
    end
end





