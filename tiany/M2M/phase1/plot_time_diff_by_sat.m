

clc
clear
close('all');

%load('training.mat');
%load('validation.mat');

%tdata=training_data;
%vdata=validation_data;

% shuffle rows 
%all0=[tdata; vdata]; 
%all=all0(randperm(size(all0, 1)), :);

load('all_sounders.mat')
all=all_sounders;


ix=1:height(all); 
all.ix=ix'; 

gcom=all( strcmp(all.imager_name, "event_GCOMW1.mat"), :);
f16=all( strcmp(all.imager_name, "event_F16.mat"), :);
f17=all( strcmp(all.imager_name, "event_F17.mat"), :);
f18=all( strcmp(all.imager_name, "event_F18.mat"), :);

figure 
plot(gcom.ix, gcom.time_diff_pixel)
hold on 
plot(f16.ix, f16.time_diff_pixel)
hold on 
plot(f17.ix, f17.time_diff_pixel)
hold on 
plot(f18.ix, f18.time_diff_pixel)

xlabel('index', 'FontSize',14);
ylabel('time diff (min)','FontSize',14);

legend("GCOM", "F16", "F17", "F18");
%exportgraphics(gcf, 'time_diff_vs_satname.png','Resolution',120);
exportgraphics(gcf, 'shuffled_time_diff_vs_satname.png','Resolution',120);


