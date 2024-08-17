
function [lon1, lon2, lat1, lat2, yyyy, mm, dd, hr, mn] = get_time_boundary_of_case(case_matf) 

% 8/16/2024 
%  Given a case .mat file, display the time and lat/lon 
% Input: 
%  case_matf: the .mat file of the case 
% Output
%  stdout 
% test case: 
%case_matf='/data1/youy/grid_satellites/noaa20-gprof-events-imager-propogated-final-land/202211/case_0020/sate_select_case_007.mat';

load(case_matf); 

lon=sate_select.lon;
lat=sate_select.lat;

lon1=min(lon, [], "all"); 
lon2=max(lon, [], "all"); 
lat1=min(lat, [], "all"); 
lat2=max(lat, [], "all"); 

[yyyy, mm, dd, hr, mn, ss]=datevec(sate_select.sate_original.time); 

% display values
%fprintf("%8.2f %8.2f %8.2f %8.2f %d %d %d %d %d\n", lon1, lon2, lat1, lat2, yyyy, mm, dd, hr, mn)

end
