%11/28/2020
clc
clear
close('all');

%***************************************************************
%data_loc_morph_result='C:\Yalei\project\morph_microwave\data\cases4paper\noaa19_result_final_step1\201502\case_0218\';
data_loc_morph_result='C:\Users\youy\work\project\morph_microwave\data\noaa19_result_final_step1\201502\case_0218\';


%case 1NOAA19 after morphed
load([data_loc_morph_result,'sate_select_case_011.mat']);

%*********************************************
%compute statistics
a2=sate_select.a2;
b2=sate_select.b2;
c2=sate_select.c2;
d2=sate_select.d2;

a2=exp(a2);
b2=exp(b2);
c2=exp(c2);
d2=exp(d2);


n1=length(a2);
rmse1=sqrt(sum((a2-b2).^2)/n1);
rmse2=sqrt(sum((a2-c2).^2)/n1);
rmse3=sqrt(sum((a2-d2).^2)/n1);

cf1=corr(a2,b2);
cf2=corr(a2,c2);
cf3=corr(a2,d2);

bias1=sum(b2-a2)/sum(a2);
bias2=sum(c2-a2)/sum(a2);
bias3=sum(d2-a2)/sum(a2);
%*********************************************

x_tick=[0.2,0.5,1,2,4,8,16,32];
x_tick=log(x_tick);
x_tickLabel={'0.2','0.5','1','2','4','8','16','32'};


figure
axes('position',[0.08,0.58,0.38,0.38]);
plot(sate_select.a2,sate_select.b2,'ob','MarkerSize',2);
hold on
% plot(sate_select.a2,sate_select.c2,'or')

axis equal tight;
grid on
xlim([x_tick(1),x_tick(end)]);
ylim([x_tick(1),x_tick(end)]);

set(gca,'xtick',x_tick);
set(gca,'xtickLabel',x_tickLabel,'FontSize',8);
set(gca,'ytick',x_tick);
set(gca,'ytickLabel',x_tickLabel,'FontSize',8)

plot([x_tick(1),x_tick(end)],[x_tick(1),x_tick(end)],'-k');
xlabel('GMI (mm/hr)','FontSize',8);
ylabel('MHS  (mm/hr)','FontSize',8);
text(0.40,0.07,'(a) Case1, Original','FontSize',8,'units','normalized');

text(0.05,0.95,'Corr.=0.52','FontSize',8,'units','normalized');
text(0.05,0.85,'RMSE=1.39 mm/hr','FontSize',8,'units','normalized');
text(0.05,0.75,'Bias=-14.07%','FontSize',8,'units','normalized');

%**************************************************************
axes('position',[0.52,0.58,0.38,0.38]);
plot(sate_select.a2,sate_select.c2,'ob','MarkerSize',2);
hold on
% plot(sate_select.a2,sate_select.c2,'or')

axis equal tight;
grid on
xlim([x_tick(1),x_tick(end)]);
ylim([x_tick(1),x_tick(end)]);

set(gca,'xtick',x_tick);
set(gca,'xtickLabel',x_tickLabel,'FontSize',8);
set(gca,'ytick',x_tick);
set(gca,'ytickLabel',x_tickLabel,'FontSize',8)

plot([x_tick(1),x_tick(end)],[x_tick(1),x_tick(end)],'-k');

xlabel('GMI (mm/hr)','FontSize',8);
ylabel('MHS morphed (mm/hr)','FontSize',8);
text(0.38,0.07,'(b) Case1, Morphed','FontSize',8,'units','normalized');

text(0.05,0.95,'Corr.=0.82','FontSize',8,'units','normalized');
text(0.05,0.85,'RMSE=0.98 mm/hr','FontSize',8,'units','normalized');
text(0.05,0.75,'Bias=-11.76%','FontSize',8,'units','normalized');
%**************************************************************************
%data_loc_morph_result='C:\Yalei\project\morph_microwave\data\cases4paper\noaa19_result_final_step1\202008\case_0599\';
data_loc_morph_result='C:\Users\youy\work\project\morph_microwave\data\noaa19_result_final_step1\202008\case_0599\';


%case 1NOAA19 after morphed
load([data_loc_morph_result,'sate_select_case_004.mat']);

%*********************************************
%compute statistics
a2=sate_select.a2;
b2=sate_select.b2;
c2=sate_select.c2;
d2=sate_select.d2;

a2=exp(a2);
b2=exp(b2);
c2=exp(c2);
d2=exp(d2);


n1=length(a2);
rmse1=sqrt(sum((a2-b2).^2)/n1);
rmse2=sqrt(sum((a2-c2).^2)/n1);
rmse3=sqrt(sum((a2-d2).^2)/n1);

cf1=corr(a2,b2);
cf2=corr(a2,c2);
cf3=corr(a2,d2);

bias1=sum(b2-a2)/sum(a2);
bias2=sum(c2-a2)/sum(a2);
bias3=sum(d2-a2)/sum(a2);
%*********************************************

axes('position',[0.08,0.08,0.38,0.38]);
plot(sate_select.a2,sate_select.b2,'ob','MarkerSize',2);
hold on
% plot(sate_select.a2,sate_select.c2,'or')

axis equal tight;
grid on
xlim([x_tick(1),x_tick(end)]);
ylim([x_tick(1),x_tick(end)]);

set(gca,'xtick',x_tick);
set(gca,'xtickLabel',x_tickLabel,'FontSize',8);
set(gca,'ytick',x_tick);
set(gca,'ytickLabel',x_tickLabel,'FontSize',8)

plot([x_tick(1),x_tick(end)],[x_tick(1),x_tick(end)],'-k');
text(0.38,0.07,'(c) Case2, Original','FontSize',8,'units','normalized');

xlabel('GMI (mm/hr)','FontSize',8);
ylabel('MHS (mm/hr)','FontSize',8);

text(0.05,0.95,'Corr.=0.52','FontSize',8,'units','normalized');
text(0.05,0.85,'RMSE=3.26 mm/hr','FontSize',8,'units','normalized');
text(0.05,0.75,'Bias=31.93%','FontSize',8,'units','normalized');


%**************************************************************
axes('position',[0.52,0.08,0.38,0.38]);
plot(sate_select.a2,sate_select.c2,'ob','MarkerSize',2);
hold on
% plot(sate_select.a2,sate_select.c2,'or')

axis equal tight;
grid on
xlim([x_tick(1),x_tick(end)]);
ylim([x_tick(1),x_tick(end)]);

set(gca,'xtick',x_tick);
set(gca,'xtickLabel',x_tickLabel,'FontSize',8);
set(gca,'ytick',x_tick);
set(gca,'ytickLabel',x_tickLabel,'FontSize',8)

plot([x_tick(1),x_tick(end)],[x_tick(1),x_tick(end)],'-k');

xlabel('GMI (mm/hr)','FontSize',8);
ylabel('MHS morphed (mm/hr)','FontSize',8);

text(0.38,0.07,'(d) Case2, Morphed','FontSize',8,'units','normalized');

text(0.05,0.95,'Corr.=0.73','FontSize',8,'units','normalized');
text(0.05,0.85,'RMSE=2.41 mm/hr','FontSize',8,'units','normalized');
text(0.05,0.75,'Bias=15.33%','FontSize',8,'units','normalized');

%*************************************************************************
set(gcf,'renderer','painter');

stop
print -depsc C:\Yalei\project\morph_microwave\figure\noaa19_case1_case3_scatter.eps
% print -depsc noaa19_case1_most_improved_gridded.eps
