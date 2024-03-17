% YDT 20230715: make density plot from two input vectors 
% Input data: 
%  entities: xdata, ydata, title
%  types:    nx1,   nx1,   string

% xdata and y data shall be log(rain_rate) for proper plotting


function plot_density (xdata, ydata, xlab, ylab, title)  

X=[xdata,  ydata]; 

% adjust colarbar limit by sample size

%over land 
N_max=round(length(xdata)/500)*2; 
%over ocean 
%N_max=round(length(xdata)/10000)*40; 
%N_max=1000; %fixed 

%don't need log anymore, as input data are log(rain_rate) 
intv=0.1/1;
sp1=-4:intv:6;
sp2=-4:intv:6;

ctrs=cell(2,1);
ctrs{1}=-4:intv:6;
ctrs{2}=-4:intv:6;

% with "edges" or without "edegs" it is only matters if we care about the
% boundary, most of times, it is fine with or without
% the following example, the N1 and N2 is exactly the same

% without edeges, N1 and N2 is slightly different, due to edge effect.
N1=hist3(X,'edges',ctrs);
N1(N1==0)=NaN;

%transerfed is a must; otherwise, x and y axis is opposite
N1=N1';
%*********************************************************
%density plot
intv=0.1/1;
sp1=-4:intv:6;
xtick1=1:1:length(sp1);
xtick2=exp(sp1);

Xs=[0.2,0.5,1,2,4,8,16,32,64,128];
yi=interp1(xtick2,xtick1,Xs);
%*******************
figure
%axes('position',[0.08,0.55,0.35,0.35]);
h=pcolor(N1);
hold on
axis equal tight
grid on
%set(h,'alphadata',~isnan(N1))
set(h, 'EdgeColor', 'none');

% turn off max color bar value
set(gca, 'CLim', [0,N_max])
set(gca,'LineWidth',0.2);

colormap jet;

hold on
plot([14,220],[14,220],'-k','LineWidth',0.1);
%********************************************************
set(gca,'xtick',yi);
set(gca,'xticklabel',Xs,'FontSize',12);

set(gca,'ytick',yi);
set(gca,'yticklabel',Xs,'FontSize',12);

xlim([yi(1),yi(end)]);
ylim([yi(1),yi(end)]);

%************************************
xlabel(xlab,'FontSize',14);
ylabel(ylab,'FontSize',14);
text(0.02,0.96,title,'FontSize',18,'units','normalized');

%********************************************
h1=colorbar;
%set(h1,'position',[0.45,0.55,0.02,0.35]);
%set(h1,'ytick',0:300:1500);
%set(h1,'ytickLabel',{'1','300','600','900','1200','1500'},'FontSize',8);

end






