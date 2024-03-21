
tail -n+2 summary.csv | while read line; do 
   dir=`echo $line | cut -f1 -d,`
   cp case_plots.html $dir/index.html

done
