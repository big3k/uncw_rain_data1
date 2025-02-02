%step4, reload data and plot each file 
%*******************************************************
function plot_step4(save_loc)

% e.g.
%save_loc='/data1/tiany/m2m-merge-version3-20241206/data/for-franklin-no-combine/step4-fixed-time-interval/'; 
%
fig_loc=[save_loc, '/new_figures/']; 
mkdir(fig_loc); 

filelist=dir([save_loc,'*.mat']);

% Some cases do not have any propagated/merged files
if  length(filelist) < 1
    disp([mfilename, ': no propagated/merged files in ', save_loc]); 
    return
end


for i=1:length(filelist)
    tp1=filelist(i).name;
    time_str=tp1(1:13); 
    % get lat/lon from filename for plotting in a 10x10 box
    [lat0, lon0] = find_latlon_from_file(tp1);
    lat1=lat0-5;   % [-90, 90] limit to be implemented 
    lat2=lat0+5; 
    lon1=lon0-5;   % [0, 360] wrap-around to be implemented 
    lon2=lon0+5;    
 
    load([save_loc, tp1]); 
    data_dir=filelist_select.folder; 
    file_name = filelist_select.name; 
    load([data_dir, '/', file_name]);  %'merged' loaded
    disp(['step4: plotting ', tp1]); 

        %YDT figure(1)
        clf; 
        figure('visible', 'off');

        %h1=pcolor(event.lon,event.lat,pr);
        h1=pcolor(merged.lon,merged.lat,merged.rate);

        set(h1,'LineStyle','None');

        hold on
        %plot(event.lon_center,event.lat_center,'+k','MarkerSize',3,'LineWidth',1)
        %plot(event.lon_bd,event.lat_bd,'-r')


        colormap jet;
        caxis([0,15]);

        axis equal tight;

        set(gca,'xtick',lon1:2:lon2);
        set(gca,'xtickLabel',lon1:2:lon2,'FontSize',8)

        set(gca,'ytick',lat1:2:lat2);
        set(gca,'ytickLabel',lat1:2:lat2,'FontSize',8)

        xlim([lon1,lon2]);
        ylim([lat1,lat2])

        xtickangle(0);
        set(gca,'LineWidth',0.2);
        %****************
        h1=colorbar;
        %YDT set(h1,'position',Cbar_POS(kk1,:));
        set(h1,'FontSize',8);
        caxis([0,15]);
        set(h1,'ytick',0:3:15);
        %******************
        set(h1,'LineWidth',0.1);

        %********************************

        title_name=time_str; 
        title(title_name,'FontSize',8)

        fig_name=[tp1, '.png']; 
        disp(['saving figure: ', fig_name]); 
        exportgraphics(gcf, [fig_loc, fig_name],'Resolution',120);

    end

end % fundtion 

