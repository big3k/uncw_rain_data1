
config_v4

CASE_YEAR='2021'; 

% get all case names
% get the folder contents
d = dir([CASE_DIR, CASE_YEAR, '/']); 
% remove all files (isdir property is 0)
dfolders = d([d(:).isdir]); 
% remove '.' and '..' 
dfolders = dfolders(~ismember({dfolders(:).name},{'.','..'})); 
% done cases to skip  
%dfolders = dfolders(~ismember({dfolders(:).name},{'FRANKLIN', 'tmp', 'ADRIAN', ...
%        'BEATRIZ', 'BIPARJOY', 'BOLAVEN', 'CALVIN', 'CHENESO'})); 

for i=1:length(dfolders) 
   disp(['Doing case: ', dfolders(i).name]);  
   CASE_NAME=dfolders(i).name; 
   do_case(CASE_YEAR, CASE_NAME) 
end 


