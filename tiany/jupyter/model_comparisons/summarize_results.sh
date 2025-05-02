# produce summary of results as html 

grep "==>" log.run_models_20250425.txt |sed -e 's/==>//' | \
  awk -F, -vOFS=, 'NR==1 {print $0, "Sample Plots"; next} 
             NR==2 {$1=""; $2=""; $3=""; print $0, ""; next} 
             NR%2==1 {print $0, "<a href=\""$1"_samples.png\">Sample plots</a>"}' > summary.csv

csv_to_html.sh summary.csv > 2models.html 





