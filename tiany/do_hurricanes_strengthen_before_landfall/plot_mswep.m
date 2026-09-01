
%(base) [tiany@itsrain sandy_plots]$ date -d '2012/10/25' +%j
%299



file = '/data2/MSWEP/V280/3hourly/2012299.21.nc'; 
% to compare with: 
% file = '/data2/satellites/imerg/1998/3B-HHR.MS.MRG.3IMERG.19981028-S210000-E212959.1260.V07B.HDF5';

% Read coordinates
lat = ncread(file,'lat');
lon = ncread(file,'lon');

lat=double(lat) 
lon=double(lon) 

% Read precipitation (time, lat, lon) in nc file 
P = ncread(file,'precipitation');

% Remove fill values
P(P < -9990) = NaN;

% IMERG is usually stored as lon x lat x time
P = squeeze(P(:,:,1))/3.0;  % mm/3h -> m/h

% flip N-S
lat = flipud(lat);
P  = P(:,end:-1:1); 
% flip (lat, lon) to (lon, lat)
P=P'; 


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

title('MSWEP Precipitation (mm/h) 2012-10-25 21:00 UTC')

exportgraphics(gcf,'mswep-20121025.png','Resolution',120)

