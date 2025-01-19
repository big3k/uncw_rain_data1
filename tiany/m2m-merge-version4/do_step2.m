%step2: use each image as an anchor, propogate all possible image (in a +/- 4 hour range) to this
%image, so later on, we can create a complete image as much as possible in
%space

% data_loc='C:\Users\youy\work\project\m2m-merge\mat\daily-gprof-20220701-grid-mat\';
% save_loc='C:\Users\youy\work\project\m2m-merge\mat\daily-gprof-20220701-grid-mat-propogated-30minutes\';

% on server, it is /data1/youy/m2m-merge-version2/daily-20230828-segment-franklin-no-combine/
%data_loc='C:\Users\youy\work\project\m2m-merge-version2\data\daily-20230828-segment-franklin-no-combine\';
%save_loc='C:\Users\youy\work\project\m2m-merge-version3-20241206\data\for-franklin-no-combine\step2-propogated-to-this-moment\';

function do_step2(data_loc, save_loc)

%data_loc='/data1/tiany/m2m-merge-version2/daily-20230828-segment-franklin-no-combine/'; 
%save_loc='/data1/tiany/m2m-merge-version3-20241206/data/for-franklin-no-combine/step2-propogated-to-this-moment/'; 
mkdir(save_loc); 
fig_loc=[save_loc, '/figures/']; 
mkdir(fig_loc); 

filelist=dir([data_loc,'*.mat']);

%get rid of the filename with TROPICS
ix1=(contains({filelist.name},{'TROPICS'}));
filelist(ix1)=[];

%******************************************************
%get time from file name
time=extractfield(filelist,'name');

tp=time{1}; %just get the first name
K2=strfind(tp,'.lon');

time=cellfun(@(a) a(K2-13:K2-1), time,'UniformOutput',false);

%keep the orginal form, so later on, we can find this file
time_file_string=time;

time=cell2mat(time');
time=datenum(time,'yyyymmdd-HHMM');
%******************************************************
%get lat/lon from the file name
location=extractfield(filelist,'name');

tp=location{1}; %just get the first name
K2=strfind(tp,'lon');
K3=strfind(tp,'lat');

lon_sign=cellfun(@(a) a(K2+4), location,'UniformOutput',false);
lon=cellfun(@(a) a(K2+5:K2+7), location,'UniformOutput',false);

lat_sign=cellfun(@(a) a(K3+4), location,'UniformOutput',false);
lat=cellfun(@(a) a(K3+5:K3+6), location,'UniformOutput',false);

lon=cell2mat(lon');
lat=cell2mat(lat');

lon_sign=cell2mat(lon_sign');
lat_sign=cell2mat(lat_sign');

lon_sign1=ones(size(lon_sign)).*(lon_sign~='n');
lon_sign1(lon_sign1==0)=-1;

lat_sign1=ones(size(lat_sign)).*(lat_sign~='n');
lat_sign1(lat_sign1==0)=-1;

lon=str2num(lon).*lon_sign1;
lat=str2num(lat).*lat_sign1;


%******************************************************
% % for i=1:length(filelist)
% %     k1=i;
% %     %k1=935;
% %
% %
% %     % figure
% %     % h1=pcolor(event.lon,event.lat,event.pr);
% %     % colormap jet;
% %     % caxis([0,15]);
% %
% %     lon1=lon(k1);
% %     lat1=lat(k1);
% %     time1=time(k1);
% %
% %     %find closest time
% %     ix1=abs(time1-time)<3/24&....
% %         abs(lon1-lon)<10&....
% %         abs(lat1-lat)<10;
% %
% %     num(i)=sum(ix1);
% %
% % end
% % % %
% % % % stop

for ijk=1:length(filelist)

  
    display([mfilename,'.m ',num2str(ijk), ' of ', num2str(length(filelist))])

    %k1=1067; %hurricane celia

    k1=ijk;


    file_name1=filelist(k1).name;
    load([data_loc,file_name1]);  

    display(['anchor file ', num2str(ijk), ':', file_name1])   % show anchor file to work on

    %event1=event;
    % clear event;
    event1=hur_case; % case file content (not "event" anymore).  
    clear hur_case; 
    event1.pr=event1.rain; 

    %YDT figure
    figure('visible', 'off');
    h1=pcolor(event1.lon,event1.lat,event1.pr);
    colormap jet;
    caxis([0,15]);
    set(h1,'LineStyle','none');

    title(file_name1,'FontSize',8)
    exportgraphics(gcf, [fig_loc, file_name1, '.png'],'Resolution',120);

    lon1=lon(k1);
    lat1=lat(k1);
    time1=time(k1);

    %find time between >0.01 hr and 4 hrs
    %use this &abs(time1-time)>=0.1/24, so that it does not include this
    %image itself

    IX1=abs(time1-time)<=4/24&abs(time1-time)>=0.1/24&....
        abs(lon1-lon)<10&....
        abs(lat1-lat)<10;

    K4=find(IX1);
    %if length(K4)<20&&length(K4)>2


    %propogate each image to this image, according to the maximum
    %correlation

    %save out the max correlation between each event, and the target event
    %then, check which event gives the largest correlation
    %1st element: moving direction, 2nd: x-dir grid; 3rd: y-dir grid; 4th:
    %ijk1, i.e., file index
  
    if length(K4)>=1

        for ijk1=1:length(K4)

            file_name2=filelist(K4(ijk1)).name;

    display(['-- time-matched file ', num2str(ijk1), ' of ', num2str(length(K4)), ': ', file_name2])   % show anchor file to work on

            s1=file_name1;
            s2=file_name2;

            %*************************************************
            %first check whereather this is enough raining pixels > 0.2
            A=event1.pr(:);

            load([data_loc,file_name2]);
            %event2=event;
            %clear event;
            event2=hur_case; 
            clear hur_case; 
            event2.pr=event2.rain; 

            B=event2.pr(:); %gprof_M is the imager closest-in-time with N19


            %only both events are 301 by 301, if not, they are at the
            %edge of 180
            if numel(A)==301*301&&numel(B)==301*301

                ix1=A>0.2&B>0.2;


                %if there is enough over-lap
                if sum(ix1)>200

                    % close('all');
                    n1=size(event1.pr,1);
                    n2=size(event1.pr,2);

                    cf1=zeros(n1,n2).*NaN;
                    cf2=cf1;
                    cf3=cf1;
                    cf4=cf1;

                    rmse1=cf1;
                    rmse2=cf2;
                    rmse3=cf3;
                    rmse4=cf4;

                    num1=cf1;
                    num2=cf1;
                    num3=cf1;
                    num4=cf1;

                    B1=event1.pr; %keep the target event "still/no-moving"
                    B1(B1<0.2)=NaN;

                    gprof_s2=event2.pr;
                    gprof_s2(gprof_s2<0.2)=NaN;

                    %only allow moving 2 degree in both lat-lon direction (i.e., 10
                    %grids)
                    for i=1:20   %41
                        %i
                        for j=1:20%161

                            %take matrix at t as anchor
                            %change matrix at t-1;

                            %*****************************************************************
                            %move the clouds north east direction
                            A1=gprof_s2;

                            t1=zeros(i,n2).*NaN;
                            A1=[t1;A1]; %add one row
                            A1(n1:(n1-1)+i,:)=[]; %get rid of the last-i row, this means that moves to north

                            t2=zeros(n1,j).*NaN;
                            A1=[t2,A1]; %add one column
                            A1(:,n2:(n2-1)+j)=[]; %get rid of the last-j column, this means move east

                            ix=~isnan(A1)&~isnan(B1);
                            a1=A1(ix);
                            b1=B1(ix);

                            if length(a1)>200
                                cf1(i,j)=corr(a1,b1,'type','pearson');

                                rmse1(i,j)=sqrt(sum((a1-b1).^2)/length(a1));
                                num1(i,j)=length(a1);

                                MA1(i,j)=prctile(a1,95);
                                MB1(i,j)=prctile(b1,95);
                            end

                            clear A1;

                            %*****************************************************************
                            %moves the clouds north-west direction
                            A1=gprof_s2; %this is rain at t-1

                            t1=zeros(i,n2).*NaN;
                            A1=[t1;A1];
                            A1(n1:(n1-1)+i,:)=[];

                            t2=zeros(n1,j).*NaN;
                            A1=[A1,t2];
                            A1(:,1:1+(j-1))=[];

                            ix=~isnan(A1)&~isnan(B1);
                            a1=A1(ix);
                            b1=B1(ix);

                            if length(a1)>200
                                cf2(i,j)=corr(a1,b1,'type','pearson');
                                rmse2(i,j)=sqrt(sum((a1-b1).^2)/length(a1));

                                num2(i,j)=length(a1);

                                MA2(i,j)=prctile(a1,95);
                                MB2(i,j)=prctile(b1,95);

                            end

                            clear A1;

                            %*****************************************************************
                            %moves the clouds south-east direction
                            A1=gprof_s2; %this is rain at t-1

                            t1=zeros(i,n2).*NaN;
                            A1=[A1;t1]; %add one row
                            A1(1:1+(i-1),:)=[]; %get rid of the first-i row, this means that moves to south

                            t2=zeros(n1,j).*NaN;
                            A1=[t2,A1]; %add one column
                            A1(:,n2:(n2-1)+j)=[]; %get rid of the last-j column, this means move east

                            ix=~isnan(A1)&~isnan(B1);
                            a1=A1(ix);
                            b1=B1(ix);

                            if length(a1)>200

                                cf3(i,j)=corr(a1,b1,'type','pearson');
                                rmse3(i,j)=sqrt(sum((a1-b1).^2)/length(a1));

                                num3(i,j)=length(a1);

                                MA3(i,j)=prctile(a1,95);
                                MB3(i,j)=prctile(b1,95);

                            end

                            clear A1;
                            %*****************************************************************
                            %moves the clouds south-west direction
                            A1=gprof_s2; %this is rain at t-1

                            t1=zeros(i,n2).*NaN;
                            A1=[A1;t1]; %add one row
                            A1(1:1+(i-1),:)=[]; %get rid of the first-i row, this means that moves to south

                            t2=zeros(n1,j).*NaN;
                            A1=[A1,t2];
                            A1(:,1:1+(j-1))=[];

                            ix=~isnan(A1)&~isnan(B1);
                            a1=A1(ix);
                            b1=B1(ix);

                            if length(a1)>200

                                cf4(i,j)=corr(a1,b1,'type','pearson');
                                rmse4(i,j)=sqrt(sum((a1-b1).^2)/length(a1));

                                num4(i,j)=length(a1);

                                MA4(i,j)=prctile(a1,95);
                                MB4(i,j)=prctile(b1,95);


                            end

                            clear A1;

                        end
                    end



                    %***************************************************************
                    %find which max is bigger from cf1 to cf4
                    cf_max=[max(cf1(:)),max(cf2(:)),max(cf3(:)),max(cf4(:))];
                    [I2,J2]=max(cf_max);

                    eval(['cf_select=cf',num2str(J2),';']);
                    %****************************************************************
                    [I3,J3]=max(cf_select(:));
                    [i2,j2]=ind2sub(size(cf2),J3);
                    i=i2;
                    j=j2;

                    adj.moving_dir=J2; %moving direction
                    adj.x_direction_grid=i2; %x-direction grid
                    adj.y_direction_grid=j2; %y-direction grid
                    adj.max_corr=I3; %max correlation
                    %*************************************************************
                    %move the propogated image to the anchor image

                    %now use the the data with zeros
                    gprof_s2=event2.pr;

                    %move the morph satellite)
                    if J2==1
                        A1=gprof_s2; %this is rain at t-1

                        t1=zeros(i,n2).*NaN;
                        A1=[t1;A1]; %add one row
                        A1(n1:(n1-1)+i,:)=[]; %get rid of the last-i row, this means that moves to north

                        t2=zeros(n1,j).*NaN;
                        A1=[t2,A1]; %add one column
                        A1(:,n2:(n2-1)+j)=[]; %get rid of the last-j column, this means move east


                        gprof_M_after=A1;
                    end


                    if J2==2
                        A1=gprof_s2; %this is rain at t-1

                        t1=zeros(i,n2).*NaN;
                        A1=[t1;A1];
                        A1(n1:(n1-1)+i,:)=[];

                        t2=zeros(n1,j).*NaN;
                        A1=[A1,t2];
                        A1(:,1:1+(j-1))=[];

                        gprof_M_after=A1;
                    end

                    if J2==3
                        A1=gprof_s2; %this is rain at t-1

                        t1=zeros(i,n2).*NaN;
                        A1=[A1;t1]; %add one row
                        A1(1:1+(i-1),:)=[]; %get rid of the first-i row, this means that moves to south

                        t2=zeros(n1,j).*NaN;
                        A1=[t2,A1]; %add one column
                        A1(:,n2:(n2-1)+j)=[]; %get rid of the last-j column, this means move east


                        gprof_M_after=A1;

                    end

                    if J2==4
                        A1=gprof_s2; %this is rain at t-1

                        t1=zeros(i,n2).*NaN;
                        A1=[A1;t1]; %add one row
                        A1(1:1+(i-1),:)=[]; %get rid of the first-i row, this means that moves to south

                        t2=zeros(n1,j).*NaN;
                        A1=[A1,t2];
                        A1(:,1:1+(j-1))=[];

                        gprof_M_after=A1;
                    end



                    %verify

                    %remember B1 is the original data
                    %gprof_M_after is propogated A1
                    a1=B1(:);
                    b1=gprof_M_after(:);

                    ix1=a1>0&b1>0;
                    a1=a1(ix1);
                    b1=b1(ix1);
                    tmpc=corr(a1,b1); 
                    display(['-- correlation: ', num2str(tmpc)]) 
                    %YDT figure
                    figure('visible', 'off');
                    plot(a1,b1,'ob')

                    %YDT figure
                    figure('visible', 'off');
                    h1=pcolor(event1.lon,event1.lat,gprof_M_after);
                    colormap jet;
                    caxis([0,15]);
                    set(h1,'LineStyle','none');

                    %combine data together
                    C1=event1.pr;
                    ix1=isnan(C1)&(~isnan(gprof_M_after));
                    C1(ix1)=gprof_M_after(ix1);

                    title(file_name2,'FontSize',8);
                    exportgraphics(gcf, [fig_loc, file_name2, '.png'],'Resolution',120);

                    %keyboard %YDT

                    %YDT figure
                    figure('visible', 'off');
                    h1=pcolor(event1.lon,event1.lat,C1);
                    colormap jet;
                    caxis([0,15]);
                    set(h1,'LineStyle','none');
                    title(file_name2,'FontSize',8); 
                    exportgraphics(gcf, [fig_loc, s1, '-', num2str(ijk1), '.png'],'Resolution',120);
                    
                    %save out the morphed image
                    moved_event.name=file_name2;
                    moved_event.rate=gprof_M_after;
                    moved_event.adj_variable=adj;
                    moved_event.time_diff=abs(time1-time(K4(ijk1)))*24*60; %minutes
                    moved_event.max_corr=corr(a1,b1);
                    
                    display([mfilename, ': --- saving file: ', s1,'-',num2str(ijk1),'.mat']) %show file saved
                    save([save_loc,s1,'-',num2str(ijk1),'.mat'],'moved_event');
                 
                    %YDT stop
                    close all;

                    clearvars -except ijk1 K4 ijk filelist file_name1 data_loc save_loc ...
                         fig_loc event1 time1 time lon lat;
                end
            end

        end

    end

    %YDT stop
    clear K4 time1 event1;
end


end %function 




