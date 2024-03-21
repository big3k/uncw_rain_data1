% YDT 20240120
% Make scatter plot for generic data, i.e., not assuming rainrates
% Input data:
%  entities: xdata, ydata, xlab, ylab, title, out_imgfile_name
%  types:    nx1,   nx1,   string, string, string, string


function plot_generic_scatter (xdata, ydata, xlab, ylab, name, out_imgfile_name) 

vmin=min([xdata; ydata]); 
vmax=max([xdata; ydata]); 

%*********************************************

x_tick=[0.2,0.5,1,2,4,8,16,32];
x_tick=log(x_tick);
x_tickLabel={'0.2','0.5','1','2','4','8','16','32'};

figure('visible', 'on');
%axes('position',[0.08,0.58,0.38,0.38]);
plot(xdata,ydata,'ob','MarkerSize',5);
hold on
% plot(xdata,sate_select.c2,'or')

axis equal tight;
grid on
xlim([vmin, vmax]);  
ylim([vmin, vmax]); 

plot([vmin, vmax],[vmin, vmax],'-k');
xlabel(xlab,'FontSize',16);
ylabel(ylab,'FontSize',16);
title(name,'FontSize',16)
%text(0.30,0.07,title,'FontSize',16,'units','normalized','Interpreter','none');
%text(0.40,0.07,title,'FontSize',16,'units','normalized','Interpreter','none');
exportgraphics(gcf, out_imgfile_name, 'Resolution',120);

end
