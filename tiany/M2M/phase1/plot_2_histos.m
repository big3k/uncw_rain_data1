% YDT 20230720: Plot two histograms 
% default range 0.1-128 in log scale
% Input data: 
%  entities: data1, data2, name1, name2 
%  types:    n,   n,   string, string

% data1 and data2 shall be log(rain_rate) for proper plotting


function plot_2_histos(data1, data2, name1, name2)  

figure 

%don't need log anymore, as input data are log(rain_rate) 
intv=0.1/1;
sp1=-3:intv:5;

histogram(data1, sp1);
hold on
histogram(data2, sp1);

Xs=[0.1, 0.2,0.5,1,2,4,8,16,32,64,128];
xticks(log(Xs))
xticklabels({'0.1', '0.2', '0.5', '1', '2', '4', '8', '16', '32', '64', '128'})
xlabel('precip. rate (mm/h)', 'FontSize',14);
ylabel('pixel count','FontSize',14);


legend(name1, name2);
title=strcat(name1, ' vs. ', name2); 
text(0.02, 0.96, title, 'FontSize',18,'units','normalized');
end






