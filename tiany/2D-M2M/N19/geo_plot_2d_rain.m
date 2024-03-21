%20240101
% plot 2D rain rate field (mm/h), with coast lines
% Input data:
%  entities: data, lat, lon, name, out_imgfile_name [,crange=[min, max]]
%  types:    NxM,  NxM, NxM, coast, string, string

function geo_plot_2d_rain(data, lat, lon, name, out_imgfile_name, options) 
   arguments
     data double
     lat double
     lon double
     name string
     out_imgfile_name string
     options.crange (1,2) double = [0, 20]; % default
end

coast=load('coastlines.mat');

figure('visible', 'off');
h1=pcolor(lon, lat, data);
set(h1,'LineStyle','none');
caxis(options.crange);
colormap jet;
colorbar;
%set(gca,'ColorScale','log');
title(name,'FontSize',16)
hold on 
plot(coast.coastlon,coast.coastlat,'-k')
axis equal tight;
xlim([min(lon(:)),max(lon(:))])
ylim([min(lat(:)),max(lat(:))])

exportgraphics(gcf, out_imgfile_name, 'Resolution',120);

end

