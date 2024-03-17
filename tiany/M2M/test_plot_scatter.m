%11/28/2020
clc
clear
close('all');

%***************************************************************
%data_loc_morph_result='C:\Yalei\project\morph_microwave\data\cases4paper\noaa19_result_final_step1\201502\case_0218\';
data_loc_morph_result='./deltatb/deltatb_ocean_2020/noaa19_result_final_step1_bk/201502/case_0218/'; 

%case 1NOAA19 after morphed
load([data_loc_morph_result,'sate_select_case_011.mat']);

%*********************************************
%compute statistics
xdata=sate_select.a2;
ydata=sate_select.b2;
%c2=sate_select.c2;
%d2=sate_select.d2;

plot_scatter(xdata, ydata, "GMI (mm/hr)", "MHS (mm/hr)", "(a) Case 1, Original") 
exportgraphics(gcf,'case1a.png','Resolution',120)

ydata=sate_select.c2;
plot_scatter(xdata, ydata, "GMI (mm/hr)", "MHS (mm/hr)", "(b) Case 1, H&H") 
exportgraphics(gcf,'case1b.png','Resolution',120)

