% YDT 20230730
% test cdf_matching function 

clc
clear
close('all');

load('training.mat'); 

n19=training_data.n19; 
gmi=training_data.gmi; 

% in log(rain)
% n19 -> gmi
trans=cdf_matching(n19, gmi); 
new_n19=interp1(trans.from, trans.to, n19); 
plot_2_histos(n19, gmi, 'n19', 'gmi'); 
plot_2_histos(new_n19, gmi, 'new_n19', 'gmi'); 


% gmi -> n19 
trans2=cdf_matching(gmi, n19); 
new_gmi=interp1(trans2.from, trans2.to, gmi); 
plot_2_histos(gmi, n19, 'gmi', 'n19'); 
plot_2_histos(new_gmi, n19, 'new_gmi', 'n19'); 

