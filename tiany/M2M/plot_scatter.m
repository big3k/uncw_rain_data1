% YDT 16230829: make scatter plot from two input vectors
% Input data:
%  entities: xdata, ydata, xlab, ylab, title
%  types:    nx1,   nx1,   string, string, string

% xdata and ydata shall be log(rain_rate) for proper plotting


function plot_scatter (xdata, ydata, xlab, ylab, title) 

xrain=exp(xdata);  % from log(rain) to rain 
yrain=exp(ydata);

n1=length(xrain);

rmse=sqrt(sum((xrain-yrain).^2)/n1);
cf=corr(xrain,yrain);
bias=sum(yrain-xrain)/sum(xrain);
%*********************************************

x_tick=[0.2,0.5,1,2,4,8,16,32];
x_tick=log(x_tick);
x_tickLabel={'0.2','0.5','1','2','4','8','16','32'};

figure('visible', 'off');
%axes('position',[0.08,0.58,0.38,0.38]);
plot(xdata,ydata,'ob','MarkerSize',5);
hold on
% plot(xdata,sate_select.c2,'or')

axis equal tight;
grid on
xlim([x_tick(1),x_tick(end)]);
ylim([x_tick(1),x_tick(end)]);

set(gca,'xtick',x_tick);
set(gca,'xtickLabel',x_tickLabel,'FontSize',16);
set(gca,'ytick',x_tick);
set(gca,'ytickLabel',x_tickLabel,'FontSize',16)

plot([x_tick(1),x_tick(end)],[x_tick(1),x_tick(end)],'-k');
xlabel(xlab,'FontSize',16);
ylabel(ylab,'FontSize',16);
text(0.30,0.07,title,'FontSize',16,'units','normalized','Interpreter','none');
%text(0.40,0.07,title,'FontSize',16,'units','normalized','Interpreter','none');

text(0.05,0.95,['Corr.=', sprintf('%.2f', cf)],'FontSize',16,'units','normalized');
text(0.05,0.85,['RMSE=', sprintf('%.2f mm/hr', rmse)],'FontSize',16,'units','normalized');
text(0.05,0.75,['Bias=', sprintf('%.2f', bias*100), '%'],'FontSize',16,'units','normalized');

end
