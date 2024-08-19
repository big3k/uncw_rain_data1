

function dump_a_case(matf, binf) 

load(matf) 

fp=fopen(binf, 'wb');
fwrite(fp, sate_select.gprof_n19, 'float32');  %  rain rate
%fwrite(fp, nvalid(nx1:nx2, ny1:ny2),'float32');  % pixel count
fclose(fp);


end 

%old examples

%load('/data1/youy/grid_satellites/noaa20-gprof-events-imager-propogated-final-land/202101/case_0048/sate_select_case_028.mat'); 

%fp=fopen("noaa20_20210101-103800.bin", 'wb');
%fwrite(fp, sate_select.gprof_n19, 'float32');  %  rain rate
%fwrite(fp, nvalid(nx1:nx2, ny1:ny2),'float32');  % pixel count
%fclose(fp);

