% YDT 20230707: changes made to run on rain. Mainly file paths reformatting. 

%11/24/2020
%analyze the final result of NOAA19

%1/16/2020
%the difference between *_bk and non-bk folder is that:
% in the bk folder (e.g.,metopa_result_final_step1_bk), I have attached
% KuPR data according to J. Munchak's suggestion

% in the non-bk folder (e.g., metopa_result_final_step1), I don't have the
% KuPR data. The differen is that: in the bk folder, I don't have 201403
% data since KuPR is not as many as GMI in that month. Very oaccuasinally,
% some cases also don't have KuPR (don't know why as of 1/16/2020).


clc
clear
close('all');

%data_loc='C:\Yalei\project\morph_microwave\data\noaa19_result_final_step1_bk\';
%data_loc='C:\Users\youy\work\project\morph_microwave\data\noaa19_result_final_step1_bk\';
%YDT 20230707: got a copy of the data on rain
data_loc='deltatb/deltatb_ocean_2020/noaa19_result_final_step1_bk/';

Nian=2014:2020;
Yue=1:12;

%****************************************************
A1=[]; %gmi
B1=[]; %N19 original
C1=[]; %N19, adjusted
D1=[]; % proporaged imager

time_diff_pixel=[]; %time difference between propoaged imager and N19
imager_id=[];   %AMSR2 or SSMISs
sys_size=[]; % precipitation system size, definded by raining pixels

%corr. between n19 and imagers. small means precip. system evolvs a lot
%do not use this kind of cases ???, or use as an adjust variable
cf_imager_n19=[]; 


jsq=0;
for ijk1=1:length(Nian)
    for ijk2=1:length(Yue)
        NianYue=[num2str(Nian(ijk1)),sprintf('%02d',Yue(ijk2))]

        %YDT data_loc1=[data_loc,NianYue,'\'];
        %YDT data_loc1=[data_loc,NianYue,'\'];
        data_loc1=[data_loc,NianYue,'/'];
        data_loc1=[data_loc,NianYue,'/'];
        case_list=dir([data_loc1,'case*']);

        if length(case_list)>0
            %****************************************************
            n1=length(case_list);
            %****************************************************
            for i=1:n1

                %YDT data_loc2=[data_loc1,case_list(i).name,'\'];
                data_loc2=[data_loc1,case_list(i).name,'/'];
                file_list=dir([data_loc2,'sate_select*.mat']);

                if length(file_list)==1
                    load([data_loc2,file_list(1).name]);

                    %     tp1=sate_select.cf(1);
                    %     tp2=sate_select.a2;
                    %
                    %     if tp1>0 && length(tp2)>50

                    if sate_select.time_diff<180

                        jsq=jsq+1;
                        cf1(jsq)=sate_select.cf(1);
                        cf2(jsq)=sate_select.cf(2);
                        time_diff(jsq)=sate_select.time_diff;

                        tp1=sate_select.name;
                        k1=strfind(tp1,'F16');
                        k2=strfind(tp1,'F17');
                        k3=strfind(tp1,'F18');
                        k4=strfind(tp1,'GCOMW');


                        if k1>0
                            sate_name(jsq)=1;
                        end

                        if k2>0
                            sate_name(jsq)=2;
                        end
                        if k3>0
                            sate_name(jsq)=3;
                        end
                        if k4>0
                            sate_name(jsq)=4;
                        end

                        %gmi data
                        A1=[A1;sate_select.a2];

                        %N19 data
                        B1=[B1;sate_select.b2];

                        %propagated imager data
                        D1=[D1;sate_select.d2];

                        %morphed/adjusted N19 data
                        %note, A1, B1, C1, D1 are in log-scale
                        % after transform to linear scale, they have the following relation:
                        % exp(B1)/2+exp(D1)/2=exp(C1)
                        C1=[C1;sate_select.c2];

                        %check
                        %[exp(B1)/2+exp(D1)/2,exp(C1)]

                        %get indicators
                        tp2=ones(size(sate_select.a2)).*sate_select.time_diff;
                        time_diff_pixel=[time_diff_pixel;tp2];

                        tp3=ones(size(sate_select.a2)).*sate_name(jsq);
                        imager_id=[imager_id;tp3];

                        tp4=ones(size(sate_select.a2)).*length(sate_select.a2);
                        sys_size=[sys_size;tp4];

                        tp5=corr(exp(B1),exp(D1));                        
                        tp6=ones(size(sate_select.a2)).*tp5;
                        cf_imager_n19=[cf_imager_n19;tp6];

                    end
                end
            end

        end
    end
end

%this is what I used in my paper, which is wrong, in log-scale
CF=[corr(A1,B1),corr(A1,C1)];

figure
hist(sate_name);

figure
hist(cf2-cf1);
sum(cf2>cf1)/length(cf1)
%*****************************************************
N_max=1000; %get proper colorbar

% % %ix1=imager_id==4;
% ix1=time_diff_pixel<30&sys_size>600;
% A1=A1(ix1);
% B1=B1(ix1);
% C1=C1(ix1);
% D1=D1(ix1);

%compute statistics
a2=A1;
b2=B1;
c2=C1;
d2=D1;

a2=exp(a2);
b2=exp(b2);
c2=exp(c2);
d2=exp(d2);

%just to verify that e2 must be c2 by definition
e2=(b2+d2)/2;

n1=length(a2);
rmse1=sqrt(sum((a2-b2).^2)/n1);
rmse2=sqrt(sum((a2-c2).^2)/n1);
rmse3=sqrt(sum((a2-d2).^2)/n1);

cf1=corr(a2,b2,'type','spearman');
cf2=corr(a2,c2,'type','spearman');
cf3=corr(a2,d2,'type','spearman');


bias1=sum(b2-a2)/sum(a2);
bias2=sum(c2-a2)/sum(a2);
bias3=sum(d2-a2)/sum(a2);
%********************************************************
%don't need log anymore, since I already log it before.
intv=0.1/1;
sp1=-4:intv:6;
sp2=-4:intv:6;

X=[A1,B1];
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
axes('position',[0.08,0.55,0.35,0.35]);
h=pcolor(N1);
hold on
axis equal tight
grid on
%set(h,'alphadata',~isnan(N1))
set(h, 'EdgeColor', 'none');

set(gca, 'CLim', [0,N_max])
set(gca,'LineWidth',0.2);

colormap jet;

hold on
plot([14,220],[14,220],'-k','LineWidth',0.1);
%********************************************************
set(gca,'xtick',yi);
set(gca,'xticklabel',Xs,'FontSize',8);

set(gca,'ytick',yi);
set(gca,'yticklabel',Xs,'FontSize',8);

xlim([yi(1),yi(end)]);
ylim([yi(1),yi(end)]);

%************************************
xlabel('GMI precip. rate (mm/hr)','FontSize',8);
ylabel('N19 precip. rate (mm/hr)','FontSize',8);
text(0.02,0.96,'(a) N19 original','FontSize',6,'units','normalized');

%********************************************
h1=colorbar;
set(h1,'position',[0.45,0.55,0.02,0.35]);
%set(h1,'ytick',0:300:1500);
%set(h1,'ytickLabel',{'1','300','600','900','1200','1500'},'FontSize',8);

%************************************************************************

%don't need log anymore, since I already log it before.
intv=0.1/1;
sp1=-4:intv:6;
sp2=-4:intv:6;

X=[A1,C1];
ctrs=cell(2,1);
ctrs{1}=-4:intv:6;
ctrs{2}=-4:intv:6;

% with "edges" or without "edegs" it is only matters if we care about the
% boundary, most of times, it is fine with or without
% the following example, the N1 and N2 is exactly the same

% without edeges, N1 and N2 is slightly different, due to edge effect.
N2=hist3(X,'edges',ctrs);
N2(N2==0)=NaN;

%transerfed is a must; otherwise, x and y axis is opposite
N2=N2';
%*********************************************************
%density plot
intv=0.1/1;
sp1=-4:intv:6;
xtick1=1:1:length(sp1);
xtick2=exp(sp1);

Xs=[0.2,0.5,1,2,4,8,16,32,64,128];
yi=interp1(xtick2,xtick1,Xs);

%************************************
axes('position',[0.56,0.55,0.35,0.35]);
h=pcolor(N2);
hold on
axis equal tight
grid on
%set(h,'alphadata',~isnan(N1))
set(h, 'EdgeColor', 'none');

set(gca, 'CLim', [0,N_max])
set(gca,'LineWidth',0.2);

colormap jet;

hold on
plot([14,220],[14,220],'-k','LineWidth',0.1);

%********************************************************
set(gca,'xtick',yi);
set(gca,'xticklabel',Xs,'FontSize',8);

set(gca,'ytick',yi);
set(gca,'yticklabel',Xs,'FontSize',8);

xlim([yi(1),yi(end)]);
ylim([yi(1),yi(end)]);


xlabel('GMI precip. rate (mm/hr)','FontSize',8);
ylabel('Adjusted N19 precip. rate (mm/hr)','FontSize',8);
text(0.02,0.96,'(b) Adjusted N19','FontSize',6,'units','normalized');

%*********************************
h1=colorbar;
set(h1,'position',[0.93,0.55,0.02,0.35]);
%set(h1,'ytick',0:300:1500);
%set(h1,'ytickLabel',{'1','300','600','900','1200','1500'},'FontSize',8);
%*************************************************************************

%don't need log anymore, since I already log it before.
intv=0.1/1;
sp1=-4:intv:6;
sp2=-4:intv:6;

X=[A1,D1];
ctrs=cell(2,1);
ctrs{1}=-4:intv:6;
ctrs{2}=-4:intv:6;

% with "edges" or without "edegs" it is only matters if we care about the
% boundary, most of times, it is fine with or without
% the following example, the N1 and N2 is exactly the same

% without edeges, N1 and N2 is slightly different, due to edge effect.
N3=hist3(X,'edges',ctrs);
N3(N3==0)=NaN;

%transerfed is a must; otherwise, x and y axis is opposite
N3=N3';
%*********************************************************
%density plot
intv=0.1/1;
sp1=-4:intv:6;
xtick1=1:1:length(sp1);
xtick2=exp(sp1);

Xs=[0.2,0.5,1,2,4,8,16,32,64,128];
yi=interp1(xtick2,xtick1,Xs);

%************************************
axes('position',[0.08,0.08,0.35,0.35]);
h=pcolor(N3);
hold on
axis equal tight
grid on
%set(h,'alphadata',~isnan(N1))
set(h, 'EdgeColor', 'none');

set(gca, 'CLim', [0,N_max])
set(gca,'LineWidth',0.2);

colormap jet;

hold on
plot([14,220],[14,220],'-k','LineWidth',0.1);

%********************************************************
set(gca,'xtick',yi);
set(gca,'xticklabel',Xs,'FontSize',8);

set(gca,'ytick',yi);
set(gca,'yticklabel',Xs,'FontSize',8);

xlim([yi(1),yi(end)]);
ylim([yi(1),yi(end)]);


xlabel('GMI precip. rate (mm/hr)','FontSize',8);
ylabel('Propogated imagers precip. rate (mm/hr)','FontSize',8);
%*********************************
h1=colorbar;
set(h1,'position',[0.43,0.05,0.02,0.35]);
% set(h1,'ytick',0:300:1500);
% set(h1,'ytickLabel',{'1','300','600','900','1200','1500'},'FontSize',8);

%*************************************************************************
%save out data for plot
result.statistics=[...
    cf1,rmse1,bias1;...
    cf2,rmse2,bias2;...
    cf3,rmse3,bias3];


result.N1=N1;
result.N2=N2;
result.N3=N3;






