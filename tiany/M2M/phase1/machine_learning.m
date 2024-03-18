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

% each has the following columns: 
% n19, imager_moved, time_diff_pixel, imager_name, cf_imager_n19, half_half, gmi

% Experiemnt with a subset of the columns  (half_half shall never be used).
training=removevars(training_data, ["imager_name", "half_half"]);
validation=removevars(validation_data, ["imager_name", "half_half"]);

if use_log == 0  % use log(rain_rate) or not 
	training.n19 = exp(training.n19); 
	training.imager_moved = exp(training.imager_moved); 
	training.gmi = exp(training.gmi); 
	validation.n19 = exp(validation.n19); 
	validation.imager_moved = exp(validation.imager_moved); 
	validation.gmi = exp(validation.gmi); 
	validation_data.half_half = exp(validation_data.half_half); 
end

% model training: linear regression
model1 = fitlm (training)  % the response variable is the last column by default.

% plot resi 
%plotResiduals(model1) 

% prediction with the validation data: 
[m2m, m2m_ci] = predict(model1, validation);

% for vlidation, always use rain_rate space
%------------------------------------------------
if use_log == 0  % use log(rain_rate) space
  m2m_rr=m2m; 
  h2h_rr=validation_data.half_half; 
  gmi_rr=validation.gmi; 
else 
  m2m_rr=exp(m2m); 
  h2h_rr=exp(validation_data.half_half); 
  gmi_rr=exp(validation.gmi); 
end

nl=length(validation.gmi); 
% compare performance m2m vs. half_half, with log(rain)
rmse0=sqrt(sum((gmi_rr-h2h_rr).^2)/nl); 
rmse1=sqrt(sum((gmi_rr-m2m_rr).^2)/nl); 

corr0=corr(validation.gmi, validation_data.half_half, 'type','spearman'); 
corr1=corr(validation.gmi, m2m, 'type','spearman'); 

bias0=sum( h2h_rr - gmi_rr ) / sum( gmi_rr ); 
bias1=sum( m2m_rr - gmi_rr ) / sum( gmi_rr ); 

perf_table=table([rmse0; rmse1], [corr0; corr1], [bias0; bias1], 'RowNames', {'half_half', 'm2m'}, ...
     'VariableNames', {'rmse', 'corr', 'bias'})


plot_density(log(gmi_rr), log(m2m_rr), "GMI precip. rate (mm/h)", "M2M Precip. rate (mm/h)", "M2M vs. GMI") 
exportgraphics(gcf,'density_m2m.png','Resolution',120)
plot_density(log(gmi_rr), log(h2h_rr), "GMI precip. rate (mm/h)", "H2H Precip. rate (mm/h)", "H2H vs. GMI")
exportgraphics(gcf,'density_h2h.png','Resolution',120)







