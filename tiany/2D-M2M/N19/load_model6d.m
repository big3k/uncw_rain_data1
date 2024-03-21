% 20140115 
% Load pre-trained models and interpolators.  
% for model6d, there are 8 models: 
%                time_diff<1hr    time_diff>=1hr
%                ------------    -------------
%                amsr2  ssmis    amsr2  ssmis
%                -----  -----    -----  -----  
%  size < 500:   exp4a, exp4b,   exp4c, exp4d 
%  size >=500:   exp5a, exp5b,   exp5c, exe5d 

% Corresponding files: 
% model_and_trans_exp4a.mat  model_and_trans_exp4b.mat  model_and_trans_exp4c.mat  model_and_trans_exp4d.mat
% model_and_trans_exp5a.mat  model_and_trans_exp5b.mat  model_and_trans_exp5c.mat  model_and_trans_exp5d.mat
% each mat file contains two variables: "model1" and "trans3". 
% model1 is used to combine sounder and imager rainrates.
% trans3 is used to map a log(rain_rate) to a new value based on pdf-matching. 

mpath="/data1/tiany/M2M/phase2/model6d/"; 

%properties of system
props=["<500 <1hr amsr2", "<500 <1hr ssmis", "<500 >=1hr amsr2", "<500 >=1hr ssmis", ...
      ">=500 <1hr amsr2", ">=500 <1hr ssmis", ">=500 >=1hr amsr2", ">=500 >=1hr ssmis"]; 
exps=["4a", "4b", "4c", "4d", "5a", "5b", "5c", "5d"]; 
idx=[1, 2, 3, 4, 5, 6, 7, 8]; 

models=cell(8); 
trans=cell(8); 

md_names=dictionary(props, exps); 
md_idxes=dictionary(props, idx); 

for i = 1:numel(props) 
   md_name=exps(i); 
   md_idx=idx(i); 
   load(mpath + "model_and_trans_exp" + md_name + ".mat"); 
   models{md_idx}=model1; 
   trans{md_idx}=trans3; 
end







