
paste -d, time_vs_meanrr_imerg.csv tc_x_25N/time_vs_meanrr_imerg.csv tc_x_30N/time_vs_meanrr_imerg.csv | \
	 awk -F, -vOFS=, '{print $1, $2, $4, $6}' > compare_time_vs_meanrr_imerg.csv 

./plot_csv.py compare_time_vs_meanrr_imerg.csv \
	    --yl "Rain Rate (mm hr^-1)" \
		    --output compare_time_vs_meanrr_imerg.png



