

# create index for each year 
out_dir=/data1/tiany/m2m-merge-version4/output
web_root=/group/m2m-merge-version4/output

#for case_year in 2022 2023; do 
for case_year in 2020; do 
   # create top index page for each year: 
   cat header.html > ${case_year}.html
   echo "<h1> Case list:</h1><br><br>" >> ${case_year}.html
   for case_dir in $out_dir/$case_year/*; do 
     case_name=`basename $case_dir` 
     echo "<a href=\"${web_root}/$case_year/$case_name/\">$case_name</a><br><br>"
   done >> ${case_year}.html
   cat footer.html >> ${case_year}.html
done 


