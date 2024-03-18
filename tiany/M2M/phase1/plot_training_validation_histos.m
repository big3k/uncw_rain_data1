% YDT 20230715
% After data are separated into two sets:  training and validation
% now start training and validating models. 

clc
clear
close('all');

% training/predict in log(rain_rate) or rain_rate space: 
% 0: don't use log; 1: use 
use_log=0; 

load('training.mat'); 
load('validation.mat'); 

% rain rates already logged 
plot_2_histos(training_data.gmi, training_data.n19, 'GMI', 'N19'); 
exportgraphics(gcf,'training_gmi_n19_histo.png','Resolution',120)
plot_2_histos(training_data.gmi, training_data.imager_moved, 'GMI', 'Imager-moved'); 
exportgraphics(gcf,'training_gmi_imager_histo.png','Resolution',120)
plot_2_histos(training_data.gmi, training_data.half_half, 'GMI', 'H2H'); 
exportgraphics(gcf,'training_gmi_h2h_histo.png','Resolution',120)

% validation
plot_2_histos(validation_data.gmi, validation_data.n19, 'GMI', 'N19'); 
exportgraphics(gcf,'validation_gmi_n19_histo.png','Resolution',120)
plot_2_histos(validation_data.gmi, validation_data.imager_moved, 'GMI', 'Imager-moved'); 
exportgraphics(gcf,'validation_gmi_imager_histo.png','Resolution',120)
plot_2_histos(validation_data.gmi, validation_data.half_half, 'GMI', 'H2H'); 
exportgraphics(gcf,'validation_gmi_h2h_histo.png','Resolution',120)
