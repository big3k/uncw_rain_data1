% 8/31/2025: YDT: find main rainrate of all timestemps, not only 0 to -60 hours. 
% output csv
% tc_name, yyyyyod, hour, rr-upto500km, filename 

%07/29/2026
%I want to find every case where the hurricane from IMERG
%remember that IMERG is 30 minute, so I can find all cases for each Hurricane


clc
clear
close all;
tic
%*********************************************
data_loc='/data1/tiany/do_hurricanes_strengthen_before_landfall/all_tcs/step1-data-mswep/old/'; 
save_loc='/data1/tiany/do_hurricanes_strengthen_before_landfall/all_tcs/step2-data-mswep/'; 

%figure name:
%YYYYMMDD-HH-MM + sensor name
%*********************************************
%YR=1998:2019; 
YR=1998; 

for ijk1=1:length(YR)

    file_list=dir([data_loc,num2str(YR(ijk1)),'/','*.mat']);
    for k2=1:length(file_list)
        %%disp([mfilename,'.m ',num2str(YR(ijk1)),' ', num2str(k2),' of ', num2str(length(file_list))]);

        dfile=[file_list(k2).folder,'/',file_list(k2).name]; 
        %%disp(['Loading ', dfile]); 

          load(dfile); 


          ix1=hur_final.d1km<=100;
          ix2=hur_final.d1km>100&hur_final.d1km<=300;
          ix3=hur_final.d1km>300&hur_final.d1km<500;

          ix4=hur_final.d1km<300;
          ix5=hur_final.d1km<500;

          rate100=hur_final.imerg_rr(ix1);
          rate300=hur_final.imerg_rr(ix2);
          rate500=hur_final.imerg_rr(ix3);

          rate_upto300=hur_final.imerg_rr(ix4);
          rate_upto500=hur_final.imerg_rr(ix5);

          % Zhong et al. conditional rain-rate threshold
          cv = 0.1;
          %% =========================================================
          % Initialize
          % ==========================================================
          rr = rate_upto500; 
          ix_rain = rr > cv;
          rr    = rr(ix_rain);
          mean_rr = mean(rr,'omitnan');
          %% fprintf('Mean rain rate = %.3f\n\n', mean_rr);

         fname = file_list(k2).name;

           tok = regexp(fname, ...
            '^([^-]+)-.*-(\d{7})\.(\d{2})\.nc*.mat$', ...
            'tokens', 'once');
           if ~isempty(tok)
            tc_name = tok{1};
            yyyyyod = tok{2};
            hour    = tok{3};

            % tc_name, yyyyyod, hour, rr-upto500km, filename 
            fprintf('%s,%s,%s,%.3f,%s\n', ...
                  tc_name,...
                  yyyyyod, ...
                  hour, ...
                  mean_rr, ...
                  fname);
          end

   end

end




