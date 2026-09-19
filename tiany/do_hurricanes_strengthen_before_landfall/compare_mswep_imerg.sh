
paste -d, compare_time_vs_meanrr.csv compare_time_vs_meanrr_imerg.csv | \
	 awk -F, -vOFS=, '{print $1, $2, $3, $4, $6, $7, $8}' > compare_time_vs_meanrr_mswep_imerg.csv 

exit
# manualllu edit the column names, then do the following 



./plot_csv.py compare_time_vs_meanrr_mswep_imerg.csv \
	    --yl "Rain Rate (mm hr^-1)" \
		    --output compare_time_vs_meanrr_mswep_imerg.png



