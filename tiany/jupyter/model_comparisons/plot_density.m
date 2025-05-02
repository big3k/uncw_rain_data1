% 20250502: 
% load data, and save two density plots into two files. 
% usage sample: 
% matlab -batch "plot_density('model_1ax_valdata.mat', 'model_1ax_density_b4.png', 'model_1ax_density_af.png')"

function plot_density(mat_file, save_b4_fig, save_af_fig) 

load(mat_file); 

%    enhanced_ssmis: [623x96x96 double]
%    original_ssmis: [623x96x96 double]
%               gmi: [623x96x96 double]
 

% 3D to 1D
ref=gmi(:);
sim1=enhanced_ssmis(:); 
sate=original_ssmis(:);

cv=0;

%ix1=ref>cv&sim1>cv&sim2>cv&sate>cv;
ix1=ref>cv&sim1>cv&sate>cv;
ref=ref(ix1);
sim1=sim1(ix1); %CNN
sate=sate(ix1);

%*****************************************************
clim_max=50;

a=log(ref);
b=log(sate);

intv=0.1/1;
sp1=-4:intv:6;
sp2=-4:intv:6;

X=[a,b];

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


%***********************************************
h=pcolor(N1);

axis equal tight
grid on
%set(h,'alphadata',~isnan(N1))
set(h, 'EdgeColor', 'none');

xtick1=1:1:length(sp1);
xtick2=exp(sp1);

Xs=[0.2,0.5,1,2,4,8,16,32,64,128,256];

yi=interp1(xtick2,xtick1,Xs);

set(gca,'xtick',yi);
set(gca,'xticklabel',Xs,'FontSize',10);

set(gca,'ytick',yi);
set(gca,'yticklabel',Xs,'FontSize',10);

xlim([yi(1),yi(end)]);
ylim([yi(1),yi(end)]);

hold on
plot([yi(1),yi(end)],[yi(1),yi(end)],'r');
h=colorbar;
% xlabel('TRMM PR (mm/hr)','Fontsize',10);
% ylabel('GPROF (mm/hr)','Fontsize',10);

hold on
plot([yi(1),yi(end)],[yi(1),yi(end)],'r');
colormap jet;

clim([0,clim_max])
exportgraphics(gcf, save_b4_fig,'Resolution',120)
%*****************************************************
%********************************a=log(ref);
b=log(sim1);

intv=0.1/1;
sp1=-4:intv:6;
sp2=-4:intv:6;

X=[a,b];

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


%***********************************************
figure
h=pcolor(N1);

axis equal tight
grid on
%set(h,'alphadata',~isnan(N1))
set(h, 'EdgeColor', 'none');

xtick1=1:1:length(sp1);
xtick2=exp(sp1);

Xs=[0.2,0.5,1,2,4,8,16,32,64,128,256];

yi=interp1(xtick2,xtick1,Xs);

set(gca,'xtick',yi);
set(gca,'xticklabel',Xs,'FontSize',10);

set(gca,'ytick',yi);
set(gca,'yticklabel',Xs,'FontSize',10);

xlim([yi(1),yi(end)]);
ylim([yi(1),yi(end)]);

hold on
plot([yi(1),yi(end)],[yi(1),yi(end)],'r');
h=colorbar;
% xlabel('TRMM PR (mm/hr)','Fontsize',10);
% ylabel('GPROF (mm/hr)','Fontsize',10);

hold on
plot([yi(1),yi(end)],[yi(1),yi(end)],'r');
colormap jet;

clim([0,clim_max])

exportgraphics(gcf, save_af_fig,'Resolution',120)

