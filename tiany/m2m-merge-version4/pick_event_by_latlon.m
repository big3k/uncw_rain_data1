
% 20250105: 
% copied from ../m2m-merge-version2/step1a_copy_the_hurricane_franklin_case_no_combine.m 

function pick_event_by_latlon(data_loc, save_loc, ...
                              CASE_DAY, CASE_LON1, CASE_LON2, CASE_LAT1, CASE_LAT2) 

%data_loc='/data1/tiany/m2m-merge-version2/daily-20230828-segment-no-combine/';
filelist=dir([data_loc,'*.mat']);
%******************************************
%save_loc='/data1/tiany/m2m-merge-version2/daily-20230828-segment-franklin-no-combine/';
mkdir(save_loc); 
%******************************************************
%get time from file name
time=extractfield(filelist,'name');

tp=time{1}; %just get the first name
K2=strfind(tp,'.lon');

time=cellfun(@(a) a(K2-13:K2-1), time,'UniformOutput',false);

%keep the orginal form, so later on, we can find this file
time_file_string=time;

time=cell2mat(time');
time=datenum(time,'yyyymmdd-HHMM');
%******************************************************
%get lat/lon from the file name
location=extractfield(filelist,'name');

tp=location{1}; %just get the first name
K2=strfind(tp,'lon');
K3=strfind(tp,'lat');

lon_sign=cellfun(@(a) a(K2+4), location,'UniformOutput',false);
lon=cellfun(@(a) a(K2+5:K2+7), location,'UniformOutput',false);

lat_sign=cellfun(@(a) a(K3+4), location,'UniformOutput',false);
lat=cellfun(@(a) a(K3+5:K3+6), location,'UniformOutput',false);

lon=cell2mat(lon');
lat=cell2mat(lat');

lon_sign=cell2mat(lon_sign');
lat_sign=cell2mat(lat_sign');

lon_sign1=ones(size(lon_sign)).*(lon_sign~='n');
lon_sign1(lon_sign1==0)=-1;

lat_sign1=ones(size(lat_sign)).*(lat_sign~='n');
lat_sign1(lat_sign1==0)=-1;

lon=str2num(lon).*lon_sign1;
lat=str2num(lat).*lat_sign1;

%******************************************************
[I,J]=sort(time,'ascend');

time=time(J);
lon=lon(J);
lat=lat(J);
filelist=filelist(J);
%******************************************************
time_interval=0:1:24; %hour
time_interval=time_interval./24;

% time_start=datenum([2023,8,28,01,00,00])+time_interval;
% time_end=datenum([2023,8,28,02,00,00])+time_interval;

% CASE_DAY +/- 1 day
time_start=datenum(CASE_DAY)-1; 
time_end=datenum(CASE_DAY)+1; 

for kk1=1%1:length(time_start)
    %ix1=time>=time_start(kk1)&time<time_end(kk1)&lon>-80&lon<-50&lat>10&lat<40;
    ix1= time >= time_start & time < time_end & ...
         lon > CASE_LON1 & lon < CASE_LON2 & ...
         lat > CASE_LAT1 & lat < CASE_LAT2; % event specific. Franklin this case 

    K1=find(ix1);

    for ijk1=1:length(K1)
        % % % figure
        %
        filelist(K1(ijk1)).name
        file_name=filelist(K1(ijk1)).name;


        % % % load([data_loc,file_name]);
        % % % 
        % % % pr=event.pr;
        % % % %pr(pr<=0.1)=NaN;
        % % % %plot(event.lon,event.lat,'.b')
        % % % 
        % % % h1=pcolor(event.lon,event.lat,pr);
        % % % set(h1,'LineStyle','None');
        % % % 
        % % % hold on
        % % % plot(event.lon_center,event.lat_center,'+m','MarkerSize',15,'LineWidth',5)
        % % % plot(event.lon_bd,event.lat_bd,'-r')
        % % % 
        % % % colormap jet;
        % % % caxis([0,10]);
        % % % 
        % % % xlim([-74,-64]);
        % % % ylim([24,36])
        % % % 
        % % % coast=load('coastlines.mat');
        % % % hold on
        % % % plot(coast.coastlon,coast.coastlat,'-k')
        %********************************************************
        %copy the the case to a new folder

        cmd1=['cp ',data_loc,file_name,' ', save_loc];
        unix(cmd1);
        %********************************************************

    end

end


