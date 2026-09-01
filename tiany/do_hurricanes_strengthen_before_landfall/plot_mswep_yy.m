
clc
clear 
close all

data_loc='/data2/MSWEP/V280/3hourly/'; 
file_name='2020248.21.nc';
files=[data_loc,file_name];

ncdisp(files)

lon=ncread(files,'lon');
lat=ncread(files,'lat');
time=ncread(files,'time');
pr=ncread(files,'precipitation');

lon=double(lon);
lat=double(lat);

%lat=flipud(lat);
%pr=flipud(pr);

time=double(time);
%days since 1900-1-1 00:00:00
%change time 
time1=time+datenum([1900,1,1,0,0,0]);



lat = flipud(lat);
pr  = pr(:,end:-1:1)/3.0;  % mm/3h -> m/h 
[X,Y] = ndgrid(lon,lat);

figure
imagesc(lon,lat,pr')
axis xy
clim([0 10])
colormap jet
colorbar

hold on

coast=load('coastlines.mat');
hold on

plot(coast.coastlon,coast.coastlat,'-r');


xlim([80,130]);
ylim([0,60]);

% % %not efficeitent for plotting
% % figure
% % h1=pcolor(X,Y,pr);
% % set(h1,'LineStyle','none');
% % clim([0 10])
% % colormap jet
% % colorbar
% % 
% % hold on
% % 
% % plot(coast.coastlon,coast.coastlat,'-r');

%compare with IMERG

file = '/data2/satellites/imerg/2020/3B-HHR.MS.MRG.3IMERG.20200904-S210000-E212959.1260.V07B.HDF5';
h5disp(file)

rr=h5read(file,'/Grid/precipitation');
%rr=h5read(file,'/Grid/Intermediate/MWprecipSource');
%rr=h5read(file,'/Grid/Intermediate/IRprecipitation');

rr(rr<0)=NaN;

lon_imerg=h5read(file,'/Grid/lon');
lat_imerg=h5read(file,'/Grid/lat');
lon_imerg=double(lon_imerg);
lat_imerg=double(lat_imerg);
[X,Y]=meshgrid(lon_imerg,lat_imerg);

figure
h1=pcolor(X,Y,rr);
set(h1,'LineStyle','none');
clim([0,10])
colormap jet;

xlim([80,130]);
ylim([0,60]);

hold on
plot(coast.coastlon,coast.coastlat,'-k')










