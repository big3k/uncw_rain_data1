
for m in 1 2 3; do 
  for loss in a b c; do 
    for act in x y; do 
     m_name="model_$m$loss$act"

     echo plotting $m_name
     matlab -batch "plot_density('${m_name}_valdata.mat', '${m_name}_density_b4.png', '${m_name}_density_af.png')"
    done
   done
done


