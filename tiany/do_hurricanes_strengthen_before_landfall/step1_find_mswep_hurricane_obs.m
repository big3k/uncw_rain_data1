% 8/16/2026: based on Yalei's version of 7/29/2026. Replace IMERG with MSWEP V280 
% keep "imerg" in all variable names. 

%07/29/2026
%I want to find every case where the hurricane from IMERG
%remember that IMERG is 30 minute, so I can find all cases for each Hurricane


clc
clear
close all;
tic
%**********************************************************
%read in Land-Ocean-Coast index data
savedata='/data2/satellites/data-from-umd-deltatb/deltatb/landocean_mask/';

fid=fopen([savedata,'xx_elevation_025.dat'],'r');
xx_ele=fread(fid,'float32');
fclose(fid);

fid=fopen([savedata,'yy_elevation_025.dat'],'r');
yy_ele=fread(fid,'float32');
fclose(fid);

fid=fopen([savedata,'LandOceanCoast_index_025_Antarctica.dat'],'r');
LOC=fread(fid,'float32');
fclose(fid);
%*********************************************

%YDT data_loc_imerg='/data2/satellites/imerg/';
% sample file: 
% /data2/MSWEP/V280/3hourly/1998218.00.nc
data_loc_imerg='/data2/MSWEP/V280/3hourly/'; 

%read out IMERG lon and lat, which are fixed values
%YDT tp='/data2/satellites/imerg/2023/3B-HHR.MS.MRG.3IMERG.20230625-S123000-E125959.0750.V07B.HDF5';
%YDT tp='/data2/satellites/3b42/2016/3B42.20161017.21.7.HDF5'; 
tp='/data2/MSWEP/V280/3hourly/1998218.00.nc'; 

%YDT lon_imerg=h5read(tp,'/Grid/lon');
%YDT lat_imerg=h5read(tp,'/Grid/lat');

lon_imerg = ncread(tp, 'lon'); 
lat_imerg = ncread(tp, 'lat'); 
% flip N-S
lat_imerg = flipud(lat_imerg);

lon_imerg=double(lon_imerg);
lat_imerg=double(lat_imerg);
[X_imerg,Y_imerg]=meshgrid(lon_imerg,lat_imerg);

X_imerg=X_imerg(:);
Y_imerg=Y_imerg(:);

%YDT data_loc='/data1/youy/project/trend-landfall/data/step0/';
%YDT save_fig_loc='/data1/youy/project/trend-landfall/data/step1-data-imerg/';
data_loc='/data1/tiany/do_hurricanes_strengthen_before_landfall/step0/'; 
save_fig_loc='/data1/tiany/do_hurricanes_strengthen_before_landfall/step1-data-mswep/';

%figure name:
%YYYYMMDD-HH-MM + sensor name
%*********************************************
YR=1998:2019;

for ijk1=1:length(YR)

    filelist_hur=dir([data_loc,num2str(YR(ijk1)),'/','*.mat']);
    %YDT-test filelist_hur=dir([data_loc,num2str(YR(ijk1)),'/','FAITH-*.mat']);

    for ijk=1:length(filelist_hur)
  
        disp(['Loading IBtracks data: ', data_loc,num2str(YR(ijk1)),'/',filelist_hur(ijk).name])

        load([data_loc,num2str(YR(ijk1)),'/',filelist_hur(ijk).name]);

        %first find unique day (i.e., loop each day)
        time=hur.time;
        lon=hur.lon;
        lat=hur.lat;
        scale=hur.scale;
        region=hur.region;

        ix1=(~isnan(lon))&(~isnan(lat))&(~isnan(time));

        time1=time(ix1);
        lon1=lon(ix1);
        lat1=lat(ix1);
        scale1=scale(ix1);
        region1=region(:,ix1);

        % Imerg filename example: 
        % /data2/satellites/imerg/1998/3B-HHR.MS.MRG.3IMERG.19981028-S210000-E212959.1260.V07B.HDF5
        % 3B42 filename example: 
        % /data2/satellites/3b42/1998/3B42.19981028.21.7.HDF5'
        %  MSWEP filename example:  yyyydoy.hr.nc 
        % /data2/MSWEP/V280/3hourly/1998218.00.nc

        %YDT time1_round=round(time1*48)/48;
        time1_round=round(time1*8)/8;  % 3-hourly instead of 30-min 
        %YDT time1_imerg=datestr(time1_round,'yyyymmdd-HHMMSS');

        dt = datetime(time1_round,'ConvertFrom','datenum');
        time1_imerg = compose('%04d%03d.%02d', ...
          year(dt), ...
          day(dt,'dayofyear'), hour(dt)); 
    
      %YDT  for k0=1:length(time1)
      %YDT    disp(['time1_imerg: ', time1_imerg(k0)]); 
      %YDT  end
      
       % keyboard

        %remember we only care about if it is in the 0-60 prior landfall
        %in the stop0, we only save out the landfall time as the last time
        %so
        landfall_prior_time=hur.time(:)-hur.time(end);
        landfall_prior_time=landfall_prior_time.*24; %change from day to hour

        for k1=1:length(time1)

            if landfall_prior_time(k1)>=-60 & landfall_prior_time(k1)<=0

                disp([mfilename,'.m ', num2str(YR(ijk1)), ' ', ...
                    num2str(ijk),' of ', num2str(length(filelist_hur)),' ', ...
                    filelist_hur(ijk).name, ' ',...
                    num2str(k1),' of ', num2str(length(time1))]);

                %YDT part_imerg_name=[time1_imerg(k1,1:8),'-S',time1_imerg(k1,10:end)];
                %YDT part_imerg_name=[time1_imerg(k1,1:8),'.',time1_imerg(k1,10:11)];  %-> 19981028.21
                part_imerg_name=[time1_imerg{k1},'.nc']; 
                file_imerg=dir([data_loc_imerg, part_imerg_name]); 
                %YDT disp(['Looking for: ', data_loc_imerg,time1_imerg(k1,1:4),'/*',part_imerg_name,'.7.HDF5']);
                disp(['Looking for: ', data_loc_imerg, part_imerg_name]);

                if ~isempty(file_imerg) & (file_imerg.bytes>1000)

                    FN_imerg=[file_imerg.folder,'/',file_imerg.name];
                    disp(['Found MSWEP: ', FN_imerg]);  %YDT
                    %YDT ir_imerg=h5read(FN_imerg,'/Grid/IRprecipitation');
                    %YDT ir_imerg(ir_imerg<0)=NaN;
                    %YDT mw_imerg=h5read(FN_imerg,'/Grid/HQprecipitation');
                    %YDT src_imerg=h5read(FN_imerg,'/Grid/satPrecipitationSource');
                    %YDT rr_imerg=h5read(FN_imerg,'/Grid/precipitation');
              
                    % Read precipitation (time, lat, lon) in nc file
                    P=ncread(FN_imerg,'precipitation');  % mm/3h 
                    P = squeeze(P(:,:,1))/3.0;  % mm/3h -> m/h
                    % flip N-S 
                    P  = P(:,end:-1:1);
                    % flip (lat, lon) to (lon, lat)
                    rr_imerg=P';
                    ir_imerg=rr_imerg;  % no data in MSWEP
                    mw_imerg=rr_imerg;  % no data in MSWEP
                    src_imerg=rr_imerg;  % no data in MSWEP

                    ir_imerg=double(ir_imerg(:));
                    mw_imerg=double(mw_imerg(:));
                    src_imerg=double(src_imerg(:));
                    rr_imerg=double(rr_imerg(:)); 

                    %to find all the pixels near hurricane 100 km, and 300 km, and 500 km, first,
                    % %I just roughly reduce the data to a 30 degree by 30 degree grid box to speed up, around the tc center

                    ix1=X_imerg>=lon1(k1)-20 & ...
                        X_imerg<=lon1(k1)+20 & ...
                        Y_imerg>=lat1(k1)-20 & ...
                        Y_imerg<=lat1(k1)+20;


                    xx1=X_imerg(ix1);yy1=Y_imerg(ix1);
                    ir_imerg=ir_imerg(ix1);
                    rr_imerg=rr_imerg(ix1);
                    mw_imerg=mw_imerg(ix1);
                    src_imerg=src_imerg(ix1);

                    latlon1=[lat1(k1),lon1(k1)];
                    latlon2=[yy1,xx1];
                    d1km=latlon2km_v2(latlon1,latlon2);

                       
%****************************************************
%add LOC
                %only save out the Land portion of the data
                %1 Land, 2 Ocean, 3 Coast

                X3=[xx_ele,yy_ele];
                Y3=[xx1,yy1];

                [IDX3,D3]=knnsearch(X3,Y3,'K',1);
                LOC1=LOC(IDX3);

%****************************************************
                    hur_final.d1km=d1km;
                    hur_final.imerg_xx=xx1;

                    hur_final.center_lon=lon1(k1);
                    hur_final.center_lat=lat1(k1);
                    hur_final.scale=scale1(k1);
                    hur_final.region=region1(:,k1);

                    hur_final.imerg_yy=yy1;
                    hur_final.imerg_ir=ir_imerg;
                    hur_final.imerg_rr=rr_imerg;
                    hur_final.imerg_mw=mw_imerg;
                    hur_final.imerg_src=src_imerg;
                    hur_final.ori_hur=hur;
                    hur_final.landfall_prior_time=landfall_prior_time(k1);
                    hur_final.LOC=LOC1;

                    save_fig_loc1=[save_fig_loc,time1_imerg{k1}(1:4),'/'];

                    cmd2=['mkdir -p ',save_fig_loc1];

                    unix(cmd2);

                    file_name_save=[filelist_hur(ijk).name,'-',file_imerg.name];

                    save([save_fig_loc1,'/',file_name_save,'.mat'],'hur_final');
                    clear file_imerg hur_final;
                    %**********************************
                end

            end

        end
    end
end


toc
t=toc






