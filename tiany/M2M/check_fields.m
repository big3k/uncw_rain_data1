% YDT 20230707: check a mat file for presense of a particular field 

clc
clear
close('all');

%data_loc='C:\Yalei\project\morph_microwave\data\noaa19_result_final_step1_bk\';
%data_loc='C:\Users\youy\work\project\morph_microwave\data\noaa19_result_final_step1_bk\';
%YDT 20230707: got a copy of the data on rain
data_loc='deltatb/deltatb_ocean_2020/noaa19_result_final_step1_bk/';

Nian=2014:2020;
Yue=1:12;

%****************************************************
A1=[]; %gmi
B1=[]; %N19 original
C1=[]; %N19, adjusted
D1=[]; % proporaged imager

time_diff_pixel=[]; %time difference between propoaged imager and N19
imager_id=[];   %AMSR2 or SSMISs
sys_size=[]; % precipitation system size, definded by raining pixels

%corr. between n19 and imagers. small means precip. system evolvs a lot
%do not use this kind of cases ???, or use as an adjust variable
cf_imager_n19=[]; 


jsq=0;
for ijk1=1:length(Nian)
    for ijk2=1:length(Yue)
        NianYue=[num2str(Nian(ijk1)),sprintf('%02d',Yue(ijk2))]

        %YDT data_loc1=[data_loc,NianYue,'\'];
        %YDT data_loc1=[data_loc,NianYue,'\'];
        data_loc1=[data_loc,NianYue,'/'];
        data_loc1=[data_loc,NianYue,'/'];
        case_list=dir([data_loc1,'case*']);

        if length(case_list)>0
            %****************************************************
            n1=length(case_list);
            %****************************************************
            for i=1:n1

                %YDT data_loc2=[data_loc1,case_list(i).name,'\'];
                data_loc2=[data_loc1,case_list(i).name,'/'];
                file_list=dir([data_loc2,'sate_select*.mat']);

                if length(file_list)==1
                    load([data_loc2,file_list(1).name]);
		    if  isfield(sate_select, 'd2')==0
		       strcat(data_loc2,file_list(1).name)
	            end

                end
            end

        end
    end
end

