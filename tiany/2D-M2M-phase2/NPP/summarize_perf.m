% 20240124
% Produce readable summary of results 

load("all_results.mat"); 

for i=1:numel(all_results.Perf_metrics)
   m2m_bias(i)=all_results.Perf_metrics(i).m2m.bias; 
   m2m_rmse(i)=all_results.Perf_metrics(i).m2m.rmse; 
   m2m_cf(i)=all_results.Perf_metrics(i).m2m.cf; 
   h2h_bias(i)=all_results.Perf_metrics(i).h2h.bias; 
   h2h_rmse(i)=all_results.Perf_metrics(i).h2h.rmse; 
   h2h_cf(i)=all_results.Perf_metrics(i).h2h.cf; 
   sensor(i)=all_results.Sys_prop(i).sensor; 
   tdiff(i)=all_results.Sys_prop(i).tdiff; 
   ssize(i)=all_results.Sys_prop(i).ssize; 
end 

sum_table=table(all_results.case_path, all_results.mat_name, ...
                                       m2m_cf', h2h_cf', ...
                                       m2m_bias', h2h_bias', ...
                                       m2m_rmse', h2h_rmse', sensor', tdiff', ssize' ); 


sum_table = renamevars(sum_table, ...
  ["Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9", "Var10", "Var11"], ...
  ["case_path", "mat_name", "m2m_cf", "h2h_cf", "m2m_bias", "h2h_bias", ...
    "m2m_rmse", "h2h_rmse", "Sensor", "Time_diff", "Sys_size"]);

writetable(sum_table, "summary.csv");

plot_generic_scatter(sum_table.h2h_cf, sum_table.m2m_cf, "H2H", "M2M", "Corr", "m2m-h2h-cf.png")

plot_generic_scatter(sum_table.h2h_bias, sum_table.m2m_bias, "H2H", "M2M", "Bias", "m2m-h2h-bias.png")

plot_generic_scatter(sum_table.h2h_rmse, sum_table.m2m_rmse, "H2H", "M2M", "RMSE", "m2m-h2h-rmse.png")



