%step4, segment merged image
%*******************************************************
function do_step3(data_loc1, save_loc)

% e.g. 
%data_loc1='/data1/tiany/m2m-merge-version3-20241206/data/for-franklin-no-combine/step3-merged-at-this-moment/'; 
%
%save_loc='/data1/tiany/m2m-merge-version3-20241206/data/for-franklin-no-combine/step4-fixed-time-interval/'; 
%
mkdir(save_loc); 
fig_loc=[save_loc, '/figures/']; 
mkdir(fig_loc); 

filelist=dir([data_loc1,'*.mat']);
%get rid of the filename with TROPICS
ix1=(contains({filelist.name},{'TROPICS'}));
filelist(ix1)=[];

%*****************************
%get time from file name
time=extractfield(filelist,'name');

tp=time{1}; %just get the first name
K2=strfind(tp,'.lon');

time=cellfun(@(a) a(K2-13:K2-1), time,'UniformOutput',false);

%keep the orginal form, so later on, we can find this file
time_file_string=time;

time=cell2mat(time');
time=datenum(time,'yyyymmdd-HHMM');
%************************************
%get sensor name from file name
%get sensor name from file name
%rember here, the sen name contains the propogated file ,and the original
%no propogation file
sen_name=zeros(size(time));

for i=1:length(filelist)
    tp1=filelist(i).name;
    K2=strfind(tp1,'.');

    TF1=contains(tp1,'TROPICS');

    if TF1
        tp2=tp1(K2(4)+1:K2(5)-3);
    else
        tp2=tp1(K2(5)+1:K2(6)-1);
    end

    tf1=strcmp(tp2,'GMI');
    tf2=strcmp(tp2,'AMSR2');
    tf3=strcmp(tp2,'SSMIS');
    tf4=strcmp(tp2,'MHS');
    tf5=strcmp(tp2,'ATMS');
    tf6=strcmp(tp2,'TROPICS');


    if tf1==1 %GMI
        sen_name(i)=1;
    end

    if tf2==1 %AMSR2
        %sen_name(i)=2;
        sen_name(i)=1;
    end

    if tf3==1 %SSMIS
        %sen_name(i)=3;
        sen_name(i)=2;

    end

    if tf4==1 %MHS
        sen_name(i)=3;%4;
    end

    if tf5==1 %ATMS
        sen_name(i)=3;%5;
    end

    if tf6==1 %TROPICS
        sen_name(i)=3;%6;
    end
end
%****************************************************************
time_interval=60;% 30 minutes
time_interval=time_interval/60/24;%change to day

%time_start=datenum([2023,08,27,12,00,00]);
%time_end=datenum([2023,08,28,24,00,0]);

%YDT: round down and up to the nearest whole hour
time_start=datenum(datestr( (min(time)-time_interval), 'yyyy-mm-dd HH:00:00')); 
time_end=datenum(datestr( (max(time)+time_interval), 'yyyy-mm-dd HH:00:00')); 

time_loop=time_start:time_interval:time_end;

for kk1=1:length(time_loop)-1
    kk1

    disp(datestr(time_loop(kk1),'yyyymmdd-HHMM'));
    time_center=(time_loop(kk1)+time_loop(kk1+1))/2;

    ix1=time>time_loop(kk1)&time<=time_loop(kk1+1);
    ix1=find(ix1);

    %1: choose GMI/AMSR2 > SSMIS > ATMS/MHS/TROPICS
    %2: if sensor the same type, choose the time close to the
    %time_center

    if length(ix1)==1
        filelist1=filelist(ix1);
        sen_name1=sen_name(ix1);
        time1=time(ix1);

        filelist_select=filelist1;

        disp(['---- Saving file ', datestr(time_loop(kk1),'yyyymmdd-HHMM'), ...
            '-',filelist_select.name,'.mat']); 
        save([save_loc,datestr(time_loop(kk1),'yyyymmdd-HHMM'),'-',filelist1.name,'.mat'],'filelist_select');

        %only one image in this hour

        %YDT figure(1)
        figure('visible', 'off');
        %YDT axes('position',POS(kk1,:));
        file_name=filelist(ix1).name;
        disp(['-- Found one file: ', file_name]); 
        load([data_loc1,file_name]);

        %pr=event.pr;
        %pr(pr<=0.1)=NaN;
        %plot(event.lon,event.lat,'.b')

        %h1=pcolor(event.lon,event.lat,pr);
        h1=pcolor(merged.lon,merged.lat,merged.rate);

        set(h1,'LineStyle','None');

        hold on
        %plot(event.lon_center,event.lat_center,'+k','MarkerSize',3,'LineWidth',1)
        %plot(event.lon_bd,event.lat_bd,'-r')


        colormap jet;
        caxis([0,15]);

        axis equal tight;

        set(gca,'xtick',-74:2:-64);
        set(gca,'xtickLabel',-74:2:-64,'FontSize',4)

        set(gca,'ytick',22:2:34);
        set(gca,'ytickLabel',22:2:34,'FontSize',4)

        xlim([-74,-64]);
        ylim([22,32])

        xtickangle(0);
        set(gca,'LineWidth',0.2);
        %****************
        h1=colorbar;
        %YDT set(h1,'position',Cbar_POS(kk1,:));
        set(h1,'FontSize',4);
        caxis([0,15]);
        set(h1,'ytick',0:3:15);
        %******************
        set(h1,'LineWidth',0.1);

        %********************************

        %add title
        %parse time and satellite name from the file name
        tp=file_name;
        if contains(tp,'TROPICS')

            K2=findstr(tp,'.');
            time_str2=tp(K2(1)+1:K2(2)-1);
            sate_name=tp(K2(4)+1:K2(5)-1);

            %creat the title
            title_name=[time_str2,'-',sate_name];
        else
            K2=findstr(tp,'.');
            time_str2=tp(K2(1)+1:K2(2)-1);
            sate_name=tp(K2(4)+1:K2(5)-1);

            %creat the title
            title_name=[time_str2,'-',sate_name];
        end
        %title(title_name,'FontSize',3.5)

        % %if it is TROPICS, make the title red
        TF2=contains(title_name,'TROPICS');
        if TF2
            title(title_name,'FontSize',3.5,'Color','red')
        else
            title(title_name,'FontSize',3.5)
        end

        fig_name=[datestr(time_loop(kk1),'yyyymmdd-HHMM'), '.png']; 
        disp(['saving figure: ', fig_name]); 
        exportgraphics(gcf, [fig_loc, fig_name],'Resolution',120);

    end

    %***********************************************************************
    if length(ix1)>1
        filelist1=filelist(ix1);
        sen_name1=sen_name(ix1);
        time1=time(ix1);
        time_diff=abs(time1-time_center).*24*60;
        disp(['-- Found ', num2str(length(ix1)), ' files']); 

        %sensor type
        ix2=sen_name1==1;
        ix3=sen_name1==2;
        ix4=sen_name1==3;

        %more than 1 images in this hour

        if sum(ix2)>=1
            time_diff1=time_diff(ix2);
            filelist2=filelist1(ix2);

            if length(filelist2)==1
                filelist_select=filelist2(1);
            else
                [I1,J1]=min(time_diff1);
                filelist_select=filelist2(J1);
            end

            disp(['---- Saving file ', datestr(time_loop(kk1),'yyyymmdd-HHMM'), ...
              '-',filelist_select.name,'.mat']); 
            save([save_loc,datestr(time_loop(kk1),'yyyymmdd-HHMM'),'-',filelist_select.name,'.mat'],'filelist_select');

            %YDT axes('position',POS(kk1,:));
            file_name=filelist_select.name;
            disp(['---- file selected: ', file_name]); 

            load([data_loc1,file_name]);

            figure('visible', 'off');
            %pr=event.pr;
            %pr(pr<=0.1)=NaN;
            %plot(event.lon,event.lat,'.b')

            %h1=pcolor(event.lon,event.lat,pr);
            h1=pcolor(merged.lon,merged.lat,merged.rate);

            set(h1,'LineStyle','None');

            hold on
            %plot(event.lon_center,event.lat_center,'+k','MarkerSize',3,'LineWidth',1)
            %plot(event.lon_bd,event.lat_bd,'-r')


            colormap jet;
            caxis([0,15]);

            axis equal tight;

            set(gca,'xtick',-74:2:-64);
            set(gca,'xtickLabel',-74:2:-64,'FontSize',4)

            set(gca,'ytick',22:2:34);
            set(gca,'ytickLabel',22:2:34,'FontSize',4)

            xlim([-74,-64]);
            ylim([22,32])

            xtickangle(0);
            set(gca,'LineWidth',0.2);
            %****************
            h1=colorbar;
            %YDT set(h1,'position',Cbar_POS(kk1,:));
            set(h1,'FontSize',4);
            caxis([0,15]);
            set(h1,'ytick',0:3:15);
            %******************
            set(h1,'LineWidth',0.1);

            %********************************

            %add title
            %parse time and satellite name from the file name
            tp=file_name;
            if contains(tp,'TROPICS')

                K2=findstr(tp,'.');
                time_str2=tp(K2(1)+1:K2(2)-1);
                sate_name=tp(K2(4)+1:K2(5)-1);

                %creat the title
                title_name=[time_str2,'-',sate_name];
            else
                K2=findstr(tp,'.');
                time_str2=tp(K2(1)+1:K2(2)-1);
                sate_name=tp(K2(4)+1:K2(5)-1);

                %creat the title
                title_name=[time_str2,'-',sate_name];
            end
            %title(title_name,'FontSize',3.5)

            % %if it is TROPICS, make the title red
            TF2=contains(title_name,'TROPICS');
            if TF2
                title(title_name,'FontSize',3.5,'Color','red')
            else
                title(title_name,'FontSize',3.5)
            end

           fig_name=[datestr(time_loop(kk1),'yyyymmdd-HHMM'), '.png']; 
           disp(['saving figure: ', fig_name]); 
           exportgraphics(gcf, [fig_loc, fig_name],'Resolution',120);

        elseif sum(ix3)>=1 %SSMIS

            time_diff1=time_diff(ix3);
            filelist2=filelist1(ix3);

            if length(filelist2)==1
                filelist_select=filelist2(1);
            else
                [I1,J1]=min(time_diff1);
                filelist_select=filelist2(J1);
            end

            disp(['---- Saving file ', datestr(time_loop(kk1),'yyyymmdd-HHMM'), ...
              '-',filelist_select.name,'.mat']); 
            save([save_loc,datestr(time_loop(kk1),'yyyymmdd-HHMM'),'-',filelist_select.name,'.mat'],'filelist_select');

            %YDT axes('position',POS(kk1,:));
            file_name=filelist_select.name;

            disp(['---- file selected: ', file_name]); 
            load([data_loc1,file_name]);

            figure('visible', 'off');
            %pr=event.pr;
            %pr(pr<=0.1)=NaN;
            %plot(event.lon,event.lat,'.b')

            %h1=pcolor(event.lon,event.lat,pr);
            h1=pcolor(merged.lon,merged.lat,merged.rate);

            set(h1,'LineStyle','None');

            hold on
            %plot(event.lon_center,event.lat_center,'+k','MarkerSize',3,'LineWidth',1)
            %plot(event.lon_bd,event.lat_bd,'-r')


            colormap jet;
            caxis([0,15]);

            axis equal tight;

            set(gca,'xtick',-74:2:-64);
            set(gca,'xtickLabel',-74:2:-64,'FontSize',4)

            set(gca,'ytick',22:2:34);
            set(gca,'ytickLabel',22:2:34,'FontSize',4)

            xlim([-74,-64]);
            ylim([22,32])

            xtickangle(0);
            set(gca,'LineWidth',0.2);
            %****************
            h1=colorbar;
            %YDT set(h1,'position',Cbar_POS(kk1,:));
            set(h1,'FontSize',4);
            caxis([0,15]);
            set(h1,'ytick',0:3:15);
            %******************
            set(h1,'LineWidth',0.1);

            %********************************

            %add title
            %parse time and satellite name from the file name
            tp=file_name;
            if contains(tp,'TROPICS')

                K2=findstr(tp,'.');
                time_str2=tp(K2(1)+1:K2(2)-1);
                sate_name=tp(K2(4)+1:K2(5)-1);

                %creat the title
                title_name=[time_str2,'-',sate_name];
            else
                K2=findstr(tp,'.');
                time_str2=tp(K2(1)+1:K2(2)-1);
                sate_name=tp(K2(4)+1:K2(5)-1);

                %creat the title
                title_name=[time_str2,'-',sate_name];
            end
            %title(title_name,'FontSize',3.5)

            % %if it is TROPICS, make the title red
            TF2=contains(title_name,'TROPICS');
            if TF2
                title(title_name,'FontSize',3.5,'Color','red')
            else
                title(title_name,'FontSize',3.5)
            end
            
            fig_name=[datestr(time_loop(kk1),'yyyymmdd-HHMM'), '.png']; 
            disp(['saving figure: ', fig_name]); 
            exportgraphics(gcf, [fig_loc, fig_name],'Resolution',120);

        else  %MHS or ATMS
            time_diff1=time_diff(ix4);
            filelist2=filelist1(ix4);

            if length(filelist2)==1
                filelist_select=filelist2(1);
            else
                [I1,J1]=min(time_diff1);
                filelist_select=filelist2(J1);
            end


            disp(['---- Saving file ', datestr(time_loop(kk1),'yyyymmdd-HHMM'), ...
              '-',filelist_select.name,'.mat']); 
            save([save_loc,datestr(time_loop(kk1),'yyyymmdd-HHMM'),'-',filelist_select.name,'.mat'],'filelist_select');

            %YDT axes('position',POS(kk1,:));
            file_name=filelist_select.name;

            disp(['---- file selected: ', file_name]); 
            load([data_loc1,file_name]);

            figure('visible', 'off');
            %pr=event.pr;
            %pr(pr<=0.1)=NaN;
            %plot(event.lon,event.lat,'.b')

            %h1=pcolor(event.lon,event.lat,pr);
            h1=pcolor(merged.lon,merged.lat,merged.rate);

            set(h1,'LineStyle','None');

            hold on
            %plot(event.lon_center,event.lat_center,'+k','MarkerSize',3,'LineWidth',1)
            %plot(event.lon_bd,event.lat_bd,'-r')


            colormap jet;
            caxis([0,15]);

            axis equal tight;

            set(gca,'xtick',-74:2:-64);
            set(gca,'xtickLabel',-74:2:-64,'FontSize',4)

            set(gca,'ytick',22:2:34);
            set(gca,'ytickLabel',22:2:34,'FontSize',4)

            xlim([-74,-64]);
            ylim([22,32])

            xtickangle(0);
            set(gca,'LineWidth',0.2);
            %****************
            h1=colorbar;
            %YDT set(h1,'position',Cbar_POS(kk1,:));
            set(h1,'FontSize',4);
            caxis([0,15]);
            set(h1,'ytick',0:3:15);
            %******************
            set(h1,'LineWidth',0.1);

            %********************************

            %add title
            %parse time and satellite name from the file name
            tp=file_name;
            if contains(tp,'TROPICS')

                K2=findstr(tp,'.');
                time_str2=tp(K2(1)+1:K2(2)-1);
                sate_name=tp(K2(4)+1:K2(5)-1);

                %creat the title
                title_name=[time_str2,'-',sate_name];
            else
                K2=findstr(tp,'.');
                time_str2=tp(K2(1)+1:K2(2)-1);
                sate_name=tp(K2(4)+1:K2(5)-1);

                %creat the title
                title_name=[time_str2,'-',sate_name];
            end
            %title(title_name,'FontSize',3.5)

            % %if it is TROPICS, make the title red
            TF2=contains(title_name,'TROPICS');
            if TF2
                title(title_name,'FontSize',3.5,'Color','red')
            else
                title(title_name,'FontSize',3.5)
            end

            fig_name=[datestr(time_loop(kk1),'yyyymmdd-HHMM'), '.png']; 
            disp(['saving figure: ', fig_name]); 
            exportgraphics(gcf, [fig_loc, fig_name],'Resolution',120);
        end
    end

    %****************************************************************
    if length(ix1)==0

        disp(['---- No file found for ', datestr(time_loop(kk1),'yyyymmdd-HHMM') ]); 
        title_name=datestr(time_loop(kk1),'yyyymmdd-HHMM'); 

        filelist1=filelist(1);

        %YDT figure(1)
        figure('visible', 'off');
        %YDT axes('position',POS(kk1,:));
        file_name=filelist(1).name;
        load([data_loc1,file_name]); %data placeholder

        %pr=event.pr;
        %pr(pr<=0.1)=NaN;
        %plot(event.lon,event.lat,'.b')

        tp=merged.rate;
        tp=tp.*NaN; %just want to add a plot

        %h1=pcolor(event.lon,event.lat,pr);
        h1=pcolor(merged.lon,merged.lat,tp);

        set(h1,'LineStyle','None');

        hold on
        %plot(event.lon_center,event.lat_center,'+k','MarkerSize',3,'LineWidth',1)
        %plot(event.lon_bd,event.lat_bd,'-r')


        colormap jet;
        caxis([0,15]);

        axis equal tight;

        set(gca,'xtick',-74:2:-64);
        set(gca,'xtickLabel',-74:2:-64,'FontSize',4)

        set(gca,'ytick',22:2:34);
        set(gca,'ytickLabel',22:2:34,'FontSize',4)

        xlim([-74,-64]);
        ylim([22,32])

        xtickangle(0);
        set(gca,'LineWidth',0.2);
        %****************
        h1=colorbar;
        %YDT set(h1,'position',Cbar_POS(kk1,:));
        set(h1,'FontSize',4);
        caxis([0,15]);
        set(h1,'ytick',0:3:15);
        %******************
        set(h1,'LineWidth',0.1);

        %********************************
        title([title_name, '-NoData'],'FontSize',3.5)

        disp(['saving ', title_name, '-NoData.png']); 
        exportgraphics(gcf, [fig_loc, title_name, '-NoData.png'],'Resolution',120);
    end

end





