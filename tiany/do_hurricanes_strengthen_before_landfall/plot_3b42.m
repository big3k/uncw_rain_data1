
file = '/data2/satellites/3b42/2012/3B42.20121025.21.7.HDF5';

% ( 1440, 400 ): (lon, lat) 
P = h5read(file,'/Grid/precipitation');
P(P < -9990) = NaN;

lon = -179.875 : 0.25 : 179.875;
lat = -49.875  : 0.25 : 49.875;

figure('visible','off',...
       'Position',[100 100 2000 1000]);

axesm('eqdcylin',...
      'MapLatLimit',[-50 50],...
      'MapLonLimit',[-180 180]);
axis off
gridm on
mlabel on
plabel on

%surfm(lat,lon,P')
surfm(lat,lon,P)

load coastlines
plotm(coastlat,coastlon,'k')

colorbar
caxis([0 16]) % MATLAB R2022a and older
title('TRMM 3B42 (mm/h) 2012-10-25 21 UTC')

exportgraphics(gcf,'3b42-20121025.png','Resolution',120)

