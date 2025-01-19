
% given a filelist, get the lat/lon values from each file; 
% Assuming all files have the same format, as long as lat/lon positions are concerned. 
% filelist[1].name  example: 
%  event.20230722-1649.lon-n050.lat-p39-GCOMW1.mat
% Usage example:
% data_loc='/data1/tiany/m2m-merge-version2/daily-20230828-segment-franklin-no-combine/'
% filelist=dir([data_loc,'*.mat']);
% [lat, lon] = find_latlon_from_filelist(filelist);   

function [lat, lon] = find_latlon_from_filelist(infile_list) 

for i=1:length(infile_list)
    disp([mfilename, ': computing lat/lon box for plotting ', infile_list(i).name]);
end

location=extractfield(infile_list,'name');

tp=location{1}; %just get the first name, assuming all files have same format
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

end 


