
paste -d, time_vs_meanrr_imerg.csv meanrr_imerg_hq.csv meanrr_imerg_ir.csv tc_x_25N/time_vs_meanrr_imerg.csv tc_x_25N/meanrr_imerg_hq.csv tc_x_25N/meanrr_imerg_ir.csv | \
	 awk -F, -vOFS=, '{print $1, $2, $4, $6, $8, $10, $12}' > compare_time_vs_meanrr_imerg_hq_ir.csv 

./plot_csv.py compare_time_vs_meanrr_imerg_hq_ir.csv \
	    --yl "Rain Rate (mm hr^-1)" \
	    --dashed 25N 25N_HQ 25N_IR \
	    --colors blue green orange blue green orange \
		    --output compare_time_vs_meanrr_imerg_hq_ir.png



