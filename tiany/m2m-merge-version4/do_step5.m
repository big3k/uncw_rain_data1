%step5, fill temporal holes

function do_step5(data_loc, save_loc) 

% e.g.
%data_loc='/data1/tiany/m2m-merge-version4/output/2023/FRANKLIN/time_sliced/'
%save_loc='/data1/tiany/m2m-merge-version4/output/2023/FRANKLIN/time_interpolated/'

mkdir(save_loc); 
fig_loc=[save_loc, '/figures/'];
mkdir(fig_loc);

filelist_all=dir([data_loc,'*.mat']);

% Compute the mean lat/lon for plotting in a 10x10 box
[lat0, lon0] = find_latlon_from_filelist(filelist_all);
clat=round(mean(lat0));
clon=round(mean(lon0));
lat1=clat-5;   % [-90, 90] limit to be implemented
lat2=clat+5;
lon1=clon-5;   % [0, 360] wrap-around to be implemented
lon2=clon+5;

%*****************************
%get time from file name
% 20230831-1900-event.20230831-1901.lon-n061.lat-p36-F18.mat-merged.mat.mat
time=extractfield(filelist_all,'name');  
time=cellfun(@(a) a(1:13), time,'UniformOutput',false); %20230828-1700

%keep the orginal form, so later on, we can find this file
time_file_string=time;
time=cell2mat(time');
time=datenum(time,'yyyymmdd-HHMM');
%****************************************************************
time_interval=60;% 30 minutes
time_interval=time_interval/60/24;%change to day

%time_start=datenum([2023,08,28,00,00,00]);
%time_end=datenum([2023,08,28,24,00,0]);
%YDT
time_start=min(time); 
time_end=max(time); 

time_loop=time_start:time_interval:time_end;

%YDT for kk1=8%1:length(time_loop)-1
for kk1=1:length(time_loop)-1  % start with 1 will crash, as no file earlier than 20230828-0000 exists 

    disp(['Processing ', num2str(kk1), ' of ', num2str(length(time_loop)-1), ...
            ' time steps. Time: ' datestr(time_loop(kk1),'yyyymmdd-HHMM')]);

    part_file_str=datestr(time_loop(kk1),'yyyymmdd-HHMM');

    filelist=dir([data_loc,part_file_str,'-*.mat']);

    %keyboard %YDT 

    if isempty(filelist)
        time_center=(time_loop(kk1)+time_loop(kk1+1))/2;

        %find two images, which have the time_center
        ix1=time_loop(kk1)>time; %last image  YDT comment: this will crash 
                                   % start_image=filelist_all(JJ1(end)); 
                                   % if ix1 is empty
        ix2=time_loop(kk1)<time; %first image

        JJ1=find(ix1);
        JJ2=find(ix2);

        %keyboard %YDT
        %start image
        start_image=filelist_all(JJ1(end));
        end_image=filelist_all(JJ2(1));

        %for now (2024/12/07), only propogate the cloeset image to the time where no PMW, later on
        %we may want to combine the propogated images

        %moved the image at the cloeset time
        %who is closer to the time_center

        time_diff1=abs(time_center-time(JJ1(end))).*24*60;
        time_diff2=abs(time_center-time(JJ2(1))).*24*60;
        
        datevec(time_center)
        datevec(time(JJ1(end)))
        datevec(time(JJ2(1)))

        load([start_image.folder,'/',start_image.name]);
        load([filelist_select.folder,'/',filelist_select.name]);

        disp(['loaded1 ', start_image.folder,'/',start_image.name]); 
        disp(['loaded2 ', filelist_select.folder,'/',filelist_select.name]); 

        %picture 1 (P1)
        P1=merged.rate;

        %plot to verify
        %YDT figure
        clf; 
        figure('visible', 'off');
        h1=pcolor(merged.lon,merged.lat,merged.rate);

        set(h1,'LineStyle','None');

        hold on
        %plot(event.lon_center,event.lat_center,'+k','MarkerSize',3,'LineWidth',1)
        %plot(event.lon_bd,event.lat_bd,'-r')


        colormap jet;
        caxis([0,15]);

        axis equal tight;

        set(gca,'xtick',-74:2:-64);
        set(gca,'xtickLabel',-74:2:-64,'FontSize',12)

        set(gca,'ytick',22:2:34);
        set(gca,'ytickLabel',22:2:34,'FontSize',12)

        xlim([lon1,lon2]);
        ylim([lat1,lat2])

        xtickangle(0);
        set(gca,'LineWidth',0.2);

        clear merged filelist_select;

        %**********************************************************
        load([end_image.folder,'/',end_image.name]);
        load([filelist_select.folder,'/',filelist_select.name]);

        disp(['loaded3 ', end_image.folder,'/',end_image.name]); 
        disp(['loaded4 ', filelist_select.folder,'/',filelist_select.name]); 

        P2=merged.rate;

        %plot to verify
        %YDT figure
        clf; 
        figure('visible', 'off');
        h1=pcolor(merged.lon,merged.lat,merged.rate);

        set(h1,'LineStyle','None');

        hold on
        %plot(event.lon_center,event.lat_center,'+k','MarkerSize',3,'LineWidth',1)
        %plot(event.lon_bd,event.lat_bd,'-r')


        colormap jet;
        caxis([0,15]);

        axis equal tight;

        set(gca,'xtick',-74:2:-64);
        set(gca,'xtickLabel',-74:2:-64,'FontSize',12)

        set(gca,'ytick',22:2:34);
        set(gca,'ytickLabel',22:2:34,'FontSize',12)

        xlim([lon1,lon2]);
        ylim([lat1,lat2])

        xtickangle(0);
        set(gca,'LineWidth',0.2);

        clear merged filelist_select;
        %***************************************************
        %move the image (or picture) closest to the time center
        % I call this image P3, the image (Picture) stays still, I call it
        % P4

        if time_diff1<time_diff2
            P3=P1; %will be moved
            P4=P2; %stay still, just to be used for motion vector calculation

            moved_image=start_image;
        else
            P3=P2;
            P4=P1;

            moved_image=end_image;
        end



        %*************************************************
        %first check whereather this is enough raining pixels > 0.2
        A=P3(:);
        B=P4(:);

        %only both events are 301 by 301, if not, they are at the
        %edge of 180
        if numel(A)==301*301&&numel(B)==301*301

            ix1=A>0.2&B>0.2;


            %if there is enough over-lap
            if sum(ix1)>200

                % close('all');
                n1=size(P3,1);
                n2=size(P3,2);

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

                B1=P4; %keep the target event "still/no-moving"
                B1(B1<0.2)=NaN;

                gprof_s2=P3;
                gprof_s2(gprof_s2<0.2)=NaN;

                %only allow moving 2 degree in both lat-lon direction (i.e., 10
                %grids)
                for i=1:20%41
                    %i
                    for j=1:20%161

                        %take matrix at t as anchor
                        %change matrix at t-1;

                        %*****************************************************************
                        %move the clouds north east direction
                        A1=gprof_s2;

                        t1=zeros(i,n2).*NaN;
                        A1=[t1;A1]; %add one row;   [nrow, ncol]
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

                %now I need to adjust the moving speed, basd on the time
                %difference


                TD_all=(time(JJ2(1))-time(JJ1(end))).*24.*60;


                if time_diff1<time_diff2
                    TD=time_diff1;
                else
                    TD=time_diff2;
                end

                ratio1=TD/TD_all;

                i=round(i*ratio1);
                j=round(j*ratio1);

                %*************************************************************
                %move the propogated image to the anchor image
                %now use the the data with zeros
                gprof_s2=P3;

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

                load([moved_image.folder,'/',moved_image.name]);
                load([filelist_select.folder,'/',filelist_select.name]);

                disp(['loaded5 ', moved_image.folder,'/',moved_image.name]);
                disp(['loaded6 ', filelist_select.folder,'/',filelist_select.name]); 

                %plot to verify
                %YDT figure
                clf; 
                figure('visible', 'off');
                h1=pcolor(merged.lon,merged.lat,gprof_M_after);

                set(h1,'LineStyle','None');

                hold on
                %plot(event.lon_center,event.lat_center,'+k','MarkerSize',3,'LineWidth',1)
                %plot(event.lon_bd,event.lat_bd,'-r')


                colormap jet;
                caxis([0,15]);

                axis equal tight;

                set(gca,'xtick',-74:2:-64);
                set(gca,'xtickLabel',-74:2:-64,'FontSize',12)

                set(gca,'ytick',22:2:34);
                set(gca,'ytickLabel',22:2:34,'FontSize',12)

                xlim([lon1,lon2]);
                ylim([lat1,lat2])

                xtickangle(0);
                set(gca,'LineWidth',0.2);

                clear merged filelist_select;

                %************************************************************
                %saved out data
                time_file_name=datestr(time_loop(kk1),'yyyymmdd-HHMM');

                name_final=[time_file_name,'-moved-from-',moved_image.name];

                %*****************************
                load([moved_image.folder,'/',moved_image.name]);
                load([filelist_select.folder,'/',filelist_select.name]);
                disp(['loaded7 ', moved_image.folder,'/',moved_image.name]);
                disp(['loaded8 ', filelist_select.folder,'/',filelist_select.name]); 
                %just want to get lon lat
                %***********************************

                moved.rate=gprof_M_after;
                moved.lon=merged.lon; 
                moved.lat=merged.lat;

                disp(['Saving ', save_loc,name_final ]); 
                save([save_loc,name_final],'moved');
                exportgraphics(gcf, [fig_loc,name_final,'.png'],'Resolution',120); 
           
               clearvars -except kk1 time_loop data_loc save_loc fig_loc time filelist_all ...
                        lat1 lat2 lon1 lon2;

               close all;

                %************************************************************
                
            end

        end
    end
end



end % function
