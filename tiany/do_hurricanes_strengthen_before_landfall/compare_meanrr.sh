
paste -d, time_vs_meanrr.csv tc_x_25N/time_vs_meanrr.csv tc_x_30N/time_vs_meanrr.csv | \
	 awk -F, -vOFS=, '{print $1, $2, $4, $6}' > compare_time_vs_meanrr.csv 

./plot_csv.py compare_time_vs_meanrr.csv \
	    --yl "Rain Rate (mm hr^-1)" \
		    --output compare_time_vs_meanrr.png



