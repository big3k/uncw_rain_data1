
file = '/data2/satellites/imerg/2012/3B-HHR.MS.MRG.3IMERG.20121025-S210000-E212959.1260.V07B.HDF5';

% Read coordinates
lat = h5read(file,'/Grid/lat');
lon = h5read(file,'/Grid/lon');

lat=double(lat) 
lon=double(lon) 

% Read precipitation ( 1, 3600, 1800 ) (time, lon, lat) 
P = h5read(file,'/Grid/precipitation');

% Remove fill values
P(P < -9990) = NaN;

% IMERG is usually stored as lon x lat x time
P = squeeze(P(:,:,1));

figure('visible','off',...
       'Position',[100 100 2000 1000]);

% Mapping Toolbox version
axesm('eqdcylin',...
      'MapLatLimit',[-60 60],...
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
caxis([0 16]) 

title('IMERG Precipitation (mm/h) 2012-10-25 21:00 UTC')
exportgraphics(gcf,'imerg-20121025.png','Resolution',120)
