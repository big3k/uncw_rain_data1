
%20250105: Original name: ../m2m-merge-version2/
%          step1_image_segment_all_sensors_no_event_combination.m

%2024/07/11
%image segmentation: segment swath into raining events (continous raining areas)

%(1) segment swath
%(2) if two events are close ( < 500 km), we combine two events

%2024/12/12/
%I don't conbine any event, only deal with the large events for now ( > 500 rainning pixels)

% how to use: 
%  do_step1(data_loc, save_loc) % 

function do_step1(data_loc, save_loc) 

intv_lon=0.1;
intv_lat=0.1;
event_size=500; %for now only care about event with raining pixels > event_size


%filelist=dir([data_loc,YM,'/','grid_*.HDF5']);
%filelist=dir([data_loc,'*TROPICS*20230828*-grid.mat']);

%filelist=dir([data_loc,'*20230802*-grid.mat']);
filelist=dir([data_loc,'*-grid.mat']); %2024/12/12, I copied the previous/after day, for interpolation

for i=1:length(filelist)

    FN=[data_loc,'/',filelist(i).name];
    
    disp([mfilename,'.m ',num2str(i), ' of ',num2str(length(filelist)), ' : ', FN]);

    load(FN);

    lon1=AV.grid_lon;
    lat1=AV.grid_lat;
    pr1=AV.grid_gprof_linear;
    time1=AV.grid_gprof_time;
    sti1=AV.grid_gprof_sti;

    % % figure
    % % h1=pcolor(lon1,lat1,pr1);
    % % set(h1,'LineStyle','none');
    % % caxis([0,5]);
    % % colormap jet;

    %Find and count connected components in binary image
    pr2=pr1;
    pr2(pr2<0.2)=0;
    pr2(pr2>=0.2)=1;

    %%without assigning 0 to NaN, doesn't work
    %Yalei's gridded data use NaN, while Yudong uses -999
    pr2(isnan(pr2))=0;

    CC= bwconncomp(pr2,8);  % matlab internal func
    cellsz=cellfun(@size,CC.PixelIdxList,'uni',false);
    cellsz1= cellfun(@(x) x(1), cellsz);

    %only deal with system large enough
    K1=find(cellsz1>event_size);
    %K1 is the location of a group of index

    %just to be consistent
    lon2=lon1;
    lat2=lat1;
    time2=time1;

    %**********************************************************************

    for j=1:length(K1)

        k3=K1(j);
        ix1=CC.PixelIdxList{k3};
        %to concatenate all cells, we need to transpose each cell so to
        %make it 1 by n, not n by 1 (???)
        %ix2=ix1';
        %ix3=cell2mat(ix2);

        lon3=lon2(ix1);
        lat3=lat2(ix1);
        time3=time2(ix1);
        pr3_all=pr1(ix1);
        pr3_gt_0p2=pr2(ix1);

        % %get boundary of the event
        %
        % k = boundary(lon3,lat3);
        % plot(lon3,lat3,'.b')
        %
        % plot(lon3(k),lat3(k),'r','LineWidth',2);

        % % % %the average center
        % % % ave_lon=mean(lon3);
        % % % ave_lat=mean(lat3);

        %2024/08/09
        %get the heavy rain center, top 15% rainfall
        P1=prctile(pr3_all,85);
        ix4=pr3_all>P1;
        ave_lon=mean(lon3(ix4));
        ave_lat=mean(lat3(ix4));

        % % % plot(ave_lon,ave_lat,'+r','MarkerSize',15,'LineWidth',3)
        % % % hold on
        % % % plot(lon3,lat3,'.b')
        % % % rectangle('Position',[ave_lon-15,ave_lat-15,30,30],'Edgecolor','r')

        %make sure each event has the same size,
        % so later on, we can easily calculate the spatial correlation

        %1. find the center lon, lat
        %2. get a 301 by 301 (15 degree by 15 degree box)
        %3. get the event
        %4. get the boundary of this event
        %5. assign NaNs to all precipitating pixels 0 outside the event boundary

        %c means center
        Ic=(ave_lon-(-180))/intv_lon;
        Jc=(ave_lat-(-90))/intv_lat;

        Ic=round(Ic);
        Jc=round(Jc);

        %get a 30 degree grid box
        I1=Ic-(15/0.1); %lon index
        I2=Ic+(15/0.1);

        J1=Jc-(15/0.1); %lat index
        J2=Jc+(15/0.1);

        %if over the edge +/-180, +/-90, I need to think some way to deal with it
        %later
        if I1<1
            I1=1;
        end

        if I2>3600
            I2=3600;
        end

        if J1<1
            J1=1;
        end

        if J2>1800
            J2=1800;
        end

        %boundary index
        bd_ix=boundary(lon3,lat3);
        lon_bd=lon3(bd_ix);
        lat_bd=lat3(bd_ix);

        %first dimension is lon, 2nd is lattitude, Yudong's data
        %first dimension is lat, 2nd is lon, Yalei'sd data

        event.lon=lon1(J1:J2,I1:I2);
        event.lat=lat1(J1:J2,I1:I2);
        event.pr=pr1(J1:J2,I1:I2);
        event.time=time1(J1:J2,I1:I2);
        event.sti=sti1(J1:J2,I1:I2);

        %get event boundary
        event.lon_bd=lon_bd;
        event.lat_bd=lat_bd;

        event.lon_center=ave_lon;
        event.lat_center=ave_lat;

        %******************************************************
        %create meaningful names
        %save out this event
        tp2=filelist(i).name;

        if  contains(tp2,'PRPS')
            sate_sensor_name=['.',tp2(1:10)]; %to be consistent with GPROF name convention
            K2=findstr(filelist(i).name,'.');
            swath_num=tp2(K2(3):K2(4));
        else
            K2=findstr(filelist(i).name,'.');
            tp1=filelist(i).name;
            sate_sensor_name=tp1(K2(1):K2(3));
            swath_num=tp1(K2(end-3):K2(end-2));
        end
        %time_str=datestr(mean(time3),'yyyymmdd-HHMM');
        %for TROPICS01, this swath TROPICS01.PRPS.L2B.Orbit11998.V04-01.ST20230828-022243.ET20230828-035700.CT20240614-212240.nc-grid.mat
        %there is a NaN value in the time3, odd (???)
        time_str=datestr(nanmean(time3),'yyyymmdd-HHMM');

        lon_str=fix(ave_lon);
        lat_str=fix(ave_lat);

        if lon_str>=0
            lon_str1=['lon-p',sprintf('%03d',lon_str)];
        else
            lon_str1=['lon-n',sprintf('%03d',abs(lon_str))];
        end

        if lat_str>=0
            lat_str1=['lat-p',sprintf('%02d',lat_str)];
        else
            lat_str1=['lat-n',sprintf('%02d',abs(lat_str))];
        end

        %save([save_loc,'/','swath',swath_num,'event.',time_str,'.',lon_str1,'.',lat_str1,sate_sensor_name,'mat'],'event');
        save([save_loc,'/','event.',time_str,'.',lon_str1,'.',lat_str1,sate_sensor_name,'swath',swath_num,'mat'],'event');

    end

    clearvars -except data_loc save_loc filelist YM ...
        intv_lon intv_lat event_size ...
        i grid_lon grid_lat ijk* YR MON;

end


toc

t=toc

% 

