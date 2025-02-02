
config_v4

CASE_YEAR='2023'; 

% get all case names
% get the folder contents
d = dir([CASE_DIR, CASE_YEAR, '/']); 
% remove all files (isdir property is 0)
dfolders = d([d(:).isdir]); 
% remove '.' and '..' 
dfolders = dfolders(~ismember({dfolders(:).name},{'.','..'})); 
% done cases to skip  
dfolders = dfolders(~ismember({dfolders(:).name},{'FRANKLIN', 'tmp', 'ADRIAN', ...
        'BEATRIZ', 'BIPARJOY', 'BOLAVEN', 'CALVIN', 'CHENESO'})); 
% cases to skip b/c they crashed
dfolders = dfolders(~ismember({dfolders(:).name},{'DAMREY'})); 

% Do cases left
dfolders = dfolders(ismember({dfolders(:).name},{ ...
'DAMREY',
'KEVIN',
'KHANUN',
'KOINU',
'LAN',
'LEE',
'LIDIA',
'LOLA',
'MARGOT',
'MAWAR',
'MOCHA',
'NIGEL',
'NORMA',
'OTIS',
'SAOLA',
'TALIM',
'TAMMY',
'TEJ'})); 

for i=1:length(dfolders) 
   disp(['Doing case: ', dfolders(i).name]);  
   CASE_NAME=dfolders(i).name; 
   do_case(CASE_YEAR, CASE_NAME) 
end 


