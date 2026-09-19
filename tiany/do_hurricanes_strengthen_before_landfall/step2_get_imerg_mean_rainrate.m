%07/29/2026
%I want to find every case where the hurricane from IMERG
%remember that IMERG is 30 minute, so I can find all cases for each Hurricane


clc
clear
close all;
tic
%*********************************************
data_loc='/data1/youy/project/trend-landfall/data/step1-data-imerg/';
save_loc='/data1/youy/project/trend-landfall/data/step2-data-imerg/';

%figure name:
%YYYYMMDD-HH-MM + sensor name
%*********************************************
%YR=1998:2025;
YR=1998;

for ijk1=1:length(YR)

     fid100=fopen([save_loc,'imerg-rainrate-100km','-',num2str(YR(ijk1)),'.dat'],'w');
     fid300=fopen([save_loc,'imerg-rainrate-300km','-',num2str(YR(ijk1)),'.dat'],'w');
     fid500=fopen([save_loc,'imerg-rainrate-500km','-',num2str(YR(ijk1)),'.dat'],'w');
     fid11=fopen([save_loc,'imerg-rainrate-upto300km','-',num2str(YR(ijk1)),'.dat'],'w');
     fid22=fopen([save_loc,'imerg-rainrate-upto500km','-',num2str(YR(ijk1)),'.dat'],'w');
     
     fid100_pt=fopen([save_loc,'imerg-rainrate-100km','-',num2str(YR(ijk1)),'-pt.dat'],'w');
     fid300_pt=fopen([save_loc,'imerg-rainrate-300km','-',num2str(YR(ijk1)),'-pt.dat'],'w');
     fid500_pt=fopen([save_loc,'imerg-rainrate-500km','-',num2str(YR(ijk1)),'-pt.dat'],'w');
     fid11_pt=fopen([save_loc,'imerg-rainrate-upto300km','-',num2str(YR(ijk1)),'-pt.dat'],'w');
     fid22_pt=fopen([save_loc,'imerg-rainrate-upto500km','-',num2str(YR(ijk1)),'-pt.dat'],'w');

     fid22_xx=fopen([save_loc,'imerg-rainrate-upto500km','-',num2str(YR(ijk1)),'-xx.dat'],'w');
     fid22_yy=fopen([save_loc,'imerg-rainrate-upto500km','-',num2str(YR(ijk1)),'-yy.dat'],'w');
     fid22_loc=fopen([save_loc,'imerg-rainrate-upto500km','-',num2str(YR(ijk1)),'-loc.dat'],'w');

    file_list=dir([data_loc,num2str(YR(ijk1)),'/','*.mat']);
    for k2=1:length(file_list)
        disp([mfilename,'.m ',num2str(YR(ijk1)),' ', num2str(k2),' of ', num2str(length(file_list))]);
          load([file_list(k2).folder,'/',file_list(k2).name]);


       if hur_final.scale>=-1


          ix1=hur_final.d1km<=100;
          ix2=hur_final.d1km>100&hur_final.d1km<=300;
          ix3=hur_final.d1km>300&hur_final.d1km<500;

          ix4=hur_final.d1km<300;
          ix5=hur_final.d1km<500;

          rate100=hur_final.imerg_rr(ix1);
          rate300=hur_final.imerg_rr(ix2);
          rate500=hur_final.imerg_rr(ix3);

          rate_upto300=hur_final.imerg_rr(ix4);
          rate_upto500=hur_final.imerg_rr(ix5);

          fwrite(fid100,rate100,'float32');
          fwrite(fid300,rate300,'float32');
          fwrite(fid500,rate500,'float32');
          fwrite(fid11,rate_upto300,'float32');
          fwrite(fid22,rate_upto500,'float32');


          %prior time landfall
          PT=ones(size(hur_final.d1km)).*hur_final.landfall_prior_time;         
          PT1=PT(ix1);
          PT2=PT(ix2);
          PT3=PT(ix3);
          PT4=PT(ix4);
          PT5=PT(ix5);

          fwrite(fid100_pt,PT1,'float32');
          fwrite(fid300_pt,PT2,'float32');
          fwrite(fid500_pt,PT3,'float32');
          fwrite(fid11_pt,PT4,'float32');
          fwrite(fid22_pt,PT5,'float32');

          xx=hur_final.imerg_xx(ix5);
          yy=hur_final.imerg_yy(ix5);
          LOC=hur_final.LOC(ix5);

          LOC5=hur_final.LOC(ix5);

          fwrite(fid22_xx,xx,'float32');
          fwrite(fid22_yy,yy,'float32');
          fwrite(fid22_loc,LOC,'float32');

          IX_error=PT5==-60&LOC5==1;

           if sum(IX_error)>100
             stop
           end
          clear hur_final;
     end

   end
 fclose('all');

end





