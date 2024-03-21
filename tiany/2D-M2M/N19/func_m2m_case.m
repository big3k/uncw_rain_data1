%20240117
% Use M2M algorithm to combine a sounder and imager data from a sate_select case 

% Input data:
%  sate_select: case data, 
%{
  struct with fields:

                    name: 'event_GCOMW1.mat'
               time_diff: 16.9860
                      cf: [0.3300 0.5300]
                      a2: [823x1 double]
                      b2: [823x1 double]
                      c2: [823x1 double]
                      d2: [823x1 double]
           gprof_n19_new: [301x301 double]
               gprof_gmi: [301x301 double]
               gprof_n19: [301x301 double]
    gprof_morphed_imager: [301x301 double]
                     lon: [301x301 double]
                     lat: [301x301 double]
              x_dir_grid: 3
              y_dir_grid: 8
                     dir: 1
            gprof_gmi_wz: [301x301 double]
            gprof_n19_wz: [301x301 double]
                 gprof_M: [301x301 double]
              gprof_M_wz: [301x301 double]
%}

% models, trans: cells of 8 elements, from "load_model6d.m"  
% md_idxes: model indexes, from "load_model6d.m"  
%
% Ouput data: 
%  m2m: [301x301 double]

function m2m=func_m2m_case(sate_select, models, trans, md_idxes) 

   % figure out which model to use, per rain system properties

         if sate_select.name=="event_GCOMW1.mat"
             sensor=" amsr2";
         else
              sensor=" ssmis";
         end

         if sate_select.time_diff < 60
              diff=" <1hr";
         else
              diff=" >=1hr";
         end

         if size(sate_select.a2) < 500
             ssize = "<500";
         else
             ssize = ">=500";
         end

         prop=ssize+diff+sensor;
         idx=md_idxes(prop);

   lmodel=models{idx}; 
   trans3=trans{idx};  % interpolator for pdf matching 

   sat=table(sate_select.gprof_n19(:), sate_select.gprof_morphed_imager(:), 'VariableNames',["sounder", "imager_moved"]);

  % model prediction
  [m2m, m2m_ci] = predict(lmodel, sat);
  
  % pdf matching
  log_m2m=interp1(trans3.from, trans3.to, log(m2m));
  m2m_1d=exp(log_m2m);
  m2m = reshape(m2m_1d, size(sate_select.gprof_morphed_imager)); 

end

