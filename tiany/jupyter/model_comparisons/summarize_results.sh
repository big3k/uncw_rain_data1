# produce summary of results as html 

grep "==>" log.run_models_20250502.txt |sed -e 's/==>//' | \
  awk -F, -vOFS=, 'NR==1 {$4="Pre/Post"; print $0, "Scatter", "Loss", "Sample"; next} 
             NR==2 {$1=""; $2=""; $3=""; print $0, "<a href=\"model_1ax_density_b4.png\">Scatter</a>","", ""; next} 
             NR%2==1 {print $0, "<a href=\""$1"_density_af.png\">Scatter</a>", "<a href=\""$1"_history.png\">Loss</a>", "<a href=\""$1"_samples.png\">Sample</a>"}' > summary.csv

csv_to_html.sh summary.csv > 3models.html 





