% YDT 20240102: Plot scatter of two 2d rain-rate fields in log scale 
% default range 0.1-128 in log scale
% Input data: 
%  entities: data1, data2, name1, name2, out_imgfile_name 
%  types:    nxm,   nxm,   string, string, string

% data1 and data2 shall be rain_rate (mm/h) for proper plotting
%
% Output data: [bias, rmse, cf]

%function [bias, rmse, cf] = plot_2d_scatter(data1, data2, name1, name2, out_imgfile_name)  
function perf = plot_2d_scatter(data1, data2, name1, name2, out_imgfile_name)  

% first mutually mask out NaNs
data1(isnan(data2))=NaN;
data2(isnan(data1))=NaN;
% convert to 1D
tmpa=data1(:);
tmpb=data2(:);
loga=log(tmpa(~isnan(tmpa)));
logb=log(tmpb(~isnan(tmpb)));
[bias, rmse, cf] = plot_scatter(loga, logb, name1, name2, "");
exportgraphics(gcf, out_imgfile_name, 'Resolution',120);
perf.bias=bias; 
perf.rmse=rmse; 
perf.cf=cf; 

end






