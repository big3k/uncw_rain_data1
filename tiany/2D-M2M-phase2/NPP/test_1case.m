
load_model6d

case_data="/data1/youy/morph_microwave_grid_gprof/npp_result_final_step1/201407/case_0377/sate_select_case_039.mat"; 

load(case_data); 

m2m=func_m2m_case(sate_select, models, trans, md_idxes); 
sate_select.m2m=m2m; % add results  
% save a copy of the data
parts=split(case_data, "/");
yymm=parts(6); 
case_dir=parts(7); 
mat_name=parts(8); 
new_dir=strcat(yymm, '/', case_dir); 

mkdir(new_dir); 
save(strcat(new_dir, '/', mat_name), "sate_select"); 

% fill NaNs with 0, then clip with gprof_n19_wz for now (need to clip with 
%  imager_morphed_wz too, if it is available ) 
m2m_wz=m2m; 
m2m_wz(isnan(m2m))=0;
m2m_wz(isnan(sate_select.gprof_n19_wz))=NaN;
h2h_wz=sate_select.gprof_n19_new;
h2h_wz(m2m_wz==0)=0;

m2m(isnan(sate_select.gprof_n19_wz))=NaN;
%{
geo_plot_2d_rain(m2m_wz, sate_select.lat, sate_select.lon, "M2M", ...
                 strcat(new_dir, '/m2m.png'), crange=[0, 9]);
geo_plot_2d_rain(sate_select.gprof_n19_wz, sate_select.lat, sate_select.lon, "N19", ...
                 strcat(new_dir, '/sdr.png'), crange=[0, 9]);
geo_plot_2d_rain(sate_select.gprof_M_wz, sate_select.lat, sate_select.lon, "IMG", ...
                 strcat(new_dir, '/img.png'), crange=[0, 9]);
geo_plot_2d_rain(h2h_wz, sate_select.lat, sate_select.lon, "H2H", ...
                 strcat(new_dir, '/h2h.png'), crange=[0, 9]);
%}

perf.sdr = plot_2d_scatter(sate_select.gprof_gmi, sate_select.gprof_n19, ...
                 "GMI", "N19", strcat(new_dir, '/sdr_vs_gmi.png')); 
perf.img = plot_2d_scatter(sate_select.gprof_gmi, sate_select.gprof_morphed_imager, ...
                 "GMI", "IMG", strcat(new_dir, '/img_vs_gmi.png')); 
perf.m2m = plot_2d_scatter(sate_select.gprof_gmi, sate_select.m2m, ...
                 "GMI", "M2M", strcat(new_dir, '/m2m_vs_gmi.png')); 
perf.h2h = plot_2d_scatter(sate_select.gprof_gmi, sate_select.gprof_n19_new, ...
                 "GMI", "H2H", strcat(new_dir, '/h2h_vs_gmi.png')); 



