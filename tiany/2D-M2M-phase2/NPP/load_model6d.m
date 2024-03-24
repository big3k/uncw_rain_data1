% 20240323 
% for NPP: 
% Load pre-trained models and interpolators.  
% for model6d, there are 3 models that are trained with enough samples: 
%                time_diff<1hr    time_diff>=1hr
%                ------------    -------------
%                amsr2  ssmis    amsr2  ssmis
%                -----  -----    -----  -----  
%  size < 500:   exp6a, n/a,     n/a,   exp6d 
%  size >=500:   exp7a, n/a,     n/a,   n/a 

% Corresponding files: 
% model_and_trans_exp6a.mat  model_and_trans_exp6d.mat
% model_and_trans_exp7a.mat  
% each mat file contains two variables: "model1" and "trans3". 
% model1 is used to combine sounder and imager rainrates.
% trans3 is used to map a log(rain_rate) to a new value based on pdf-matching. 

mpath="/data1/tiany/M2M/phase2/model6d/"; 

%properties of system
props=["<500 <1hr amsr2", "<500 <1hr ssmis", "<500 >=1hr amsr2", "<500 >=1hr ssmis", ...
      ">=500 <1hr amsr2", ">=500 <1hr ssmis", ">=500 >=1hr amsr2", ">=500 >=1hr ssmis"]; 
exps=["6a", "null", "null", "6d", "7a", "null", "null", "null"]; 
idx=[1, 2, 3, 4, 5, 6, 7, 8]; 

models=cell(8); 
trans=cell(8); 

md_names=dictionary(props, exps); 
md_idxes=dictionary(props, idx); 

for i = 1:numel(props) 
   md_name=exps(i); 
   md_idx=idx(i); 
   if ~strcmp(md_name, "null")
    load(mpath + "model_and_trans_exp" + md_name + ".mat"); 
    models{md_idx}=model1; 
    trans{md_idx}=trans3; 
   else 
    models{md_idx}=missing; 
    trans{md_idx}=missing; 
   end
end







