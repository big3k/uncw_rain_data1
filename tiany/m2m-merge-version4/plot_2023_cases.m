
config_v4

CASE_YEAR='2023'; 

% get all case names
% get the folder contents
d = dir([CASE_DIR, CASE_YEAR, '/']); 
% remove all files (isdir property is 0)
dfolders = d([d(:).isdir]); 
% remove '.' and '..' 
dfolders = dfolders(~ismember({dfolders(:).name},{'.','..', 'tmp'})); 
% Do cases left
dfolders = dfolders(ismember({dfolders(:).name},{'FRANKLIN' ...
}));


for i=1:length(dfolders) 
   disp(['Plotting case: ', dfolders(i).name]);  
   CASE_NAME=dfolders(i).name; 
   plot_case(CASE_YEAR, CASE_NAME) 
end 


