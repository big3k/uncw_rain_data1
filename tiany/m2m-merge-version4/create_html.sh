

# create index for each year 
out_dir=/data1/tiany/m2m-merge-version4/output
web_root=/group/m2m-merge-version4/output

for case_year in 2023; do 
   # create top index page for each year: 
   cat header.html > ${case_year}.html
   echo "<h1> Case list:</h1><br><br>" >> ${case_year}.html
   for case_dir in $out_dir/$case_year/*; do 
     case_name=`basename $case_dir` 
     echo "<a href=\"${web_root}/$case_year/$case_name/\">$case_name</a><br><br>"
   done >> ${case_year}.html
   cat footer.html >> ${case_year}.html
done 


# create index for each case 
for case_year in 2023; do 
   for case_dir in $out_dir/$case_year/*; do 
     case_name=`basename $case_dir` 
     cat header.html > $case_dir/index.html
     # find all the time sliced and interpolated images and figure out the 
     # time range 
     # sample png file format: 
     # 20231013-2000-moved-from....png 
     ( 
      cd $case_dir/time_sliced/figures/; ls
      cd $case_dir/time_interpolated/figures/; ls 
      ) | cut -c1-13 |sort -n -u |while read hourly; do 
          echo $case_name $hourly 
      done

    done # case -dir
done # case_year


exit


