%step3, merge at PMW moment

function do_step3(data_loc1, data_loc2, save_loc)

%data_loc1='/data1/tiany/m2m-merge-version2/daily-20230828-segment-franklin-no-combine/'; 
%data_loc2='/data1/tiany/m2m-merge-version3-20241206/data/for-franklin-no-combine/step2-propogated-to-this-moment/'; 

%save_loc='/data1/tiany/m2m-merge-version3-20241206/data/for-franklin-no-combine/step3-merged-at-this-moment/'; 
mkdir(save_loc); 
fig_loc=[save_loc, '/figures/']; 
mkdir(fig_loc); 

filelist1=dir([data_loc1,'*.mat']);
%get rid of the filename with TROPICS
ix1=(contains({filelist1.name},{'TROPICS'}));
filelist1(ix1)=[];

%this is moved image to this moment
filelist2=dir([data_loc2,'*.mat']);


for ijk=1:length(filelist1)

    disp([mfilename, ': Doing file #', num2str(ijk), ' of ', num2str(length(filelist1))]); 

    file_name1=filelist1(ijk).name;
    disp(['loading segmented file: ', data_loc1, file_name1]); 
    load([data_loc1,file_name1]);

    event=hur_case; % case file content (not "event" anymore).
    clear hur_case;
    event.pr=event.rain;

    A1=event.pr;
    lon=event.lon;
    lat=event.lat;

    %YDT figure
    figure('visible', 'off');
    
    h1=pcolor(lon,lat,A1);
    colormap jet;
    caxis([0,15]);
    set(h1,'LineStyle','none');

    %find moved image to this moment
    ix2=(contains({filelist2.name},{file_name1}));

    K1=find(ix2);
    if length(K1)>0
        
        pr_moved_all=[];
        ind_moved_all=[];
        time_diff_all=[];
        max_corr_all=[];

        for i=1:length(K1)

            disp(['loading propagated file: ', data_loc2, filelist2(K1(i)).name]); 
            load([data_loc2,filelist2(K1(i)).name]);

            %YDT figure
            %figure('visible', 'off');
            %h1=pcolor(lon,lat,moved_event.rate);
            %colormap jet;
            %caxis([0,15]);
            %set(h1,'LineStyle','none');

            rate=moved_event.rate;
            tmp=moved_event.time_diff.*ones(size(rate));
            tmp1=moved_event.max_corr.*ones(size(rate));
            

            pr_moved_all=[pr_moved_all;rate(:)];
            
            tmp_ind=1:length(rate(:));
            tmp_ind=tmp_ind';
            ind_moved_all=[ind_moved_all;tmp_ind];
            time_diff_all=[time_diff_all;tmp(:)];
            max_corr_all=[max_corr_all;tmp1(:)];            
        end

        %now combine all data together, 
        %reshape the original image to 1-D first
        rate_all=[A1(:);pr_moved_all];

        %generate linear index
        tmp1=1:length(A1(:));
        tmp1=tmp1';
        ind_all=[tmp1;ind_moved_all];        
        time_all=[zeros(size(tmp1));time_diff_all];
        corr_all=[ones(size(tmp1));max_corr_all];

        %create the combined precip. rate
        C1=A1;

        for i=1:length(A1(:))
            tmp2=A1(i);

            if isnan(tmp2)

                ix1=ind_all==i;
                R1=rate_all(ix1);
                T1=time_all(ix1);
                CF1=corr_all(ix1);
             
                ix2=~isnan(R1);
                if sum(ix2)>0
                    R2=R1(ix2);
                    T2=T1(ix2);
                    CF2=CF1(ix2);

                    %[I1,J1]=min(T2); %minmum time
                    [I1,J1]=max(CF2); %max spatial corr.

                    C1(i)=R2(J1);
                end

            end
        end

        %YDT figure
        figure('visible', 'off');
        h1=pcolor(lon,lat,C1);
        colormap jet;
        caxis([0,15]);
        set(h1,'LineStyle','none');
        title(file_name1,'FontSize',8);
        exportgraphics(gcf, [fig_loc, file_name1, '-merged.png'],'Resolution',120);

        merged.rate=C1;
        merged.lon=lon;
        merged.lat=lat;

        %YDT stop
        %save out the merged PMW data
        close all;
        disp(['saving merged file: ', save_loc, file_name1, '-merged.mat']); 
        save([save_loc,file_name1,'-merged.mat'],'merged');

        clearvars -except data_loc1 data_loc2 save_loc fig_loc filelist1 filelist2 ijk;

    end

end

end % function 

