
clc
clear
close('all');

FN=['./test2.nc'];

ncdisp(FN);

lat=ncread(FN,'/Geolocation_Time_Fields/latitude_a2');
lon=ncread(FN,'/Geolocation_Time_Fields/longitude_a2');

st=ncread(FN,'/Data_Fields/surface_type_a2');
st=double(st);

%coast=load('coastlines.mat');

figure(1)
%plot(coast.coastlon,coast.coastlat,'-k')
hold on
plot(lon,lat,'.b');
plot(lon(st==1),lat(st==1),'.r') 

plot(lon(st==2),lat(st==2),'.g') 

