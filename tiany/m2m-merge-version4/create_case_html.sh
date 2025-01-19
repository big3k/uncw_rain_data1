out_dir=/data1/tiany/m2m-merge-version4/output

# create index for each case 
for case_year in 2023; do 
   for case_dir in $out_dir/$case_year/*; do 
     echo $case_dir  
     case_name=`basename $case_dir` 
     cat header.html > $case_dir/index.html
     echo "<h1>$case_year $case_name</h1>" >> $case_dir/index.html
     echo "<table class=\"table table-striped\"><tbody>" >> $case_dir/index.html
     # find all the time sliced and interpolated images and figure out the 
     # time range 
     # sample png file format: 
     # 20231013-2000-moved-from....png 
     ncol=1
     ( 
      cd $case_dir/time_sliced/figures/; ls
      cd $case_dir/time_interpolated/figures/; ls 
      ) | cut -c1-13 |sort -n |uniq |while read hourly; do 
        echo $case_name $hourly 
        ts_png=`cd $case_dir/time_sliced/figures/; ls ${hourly}*.png`  
        ti_png=`cd $case_dir/time_interpolated/figures/; ls ${hourly}*.png`  
      if [ $ncol -eq 1 ]; then 
        echo "<tr><td><br><br><br><br>Time Sliced<br><br> vs. <br><br>Time Interpolated</td>" >> $case_dir/index.html
      fi
        cat >> $case_dir/index.html <<EOF
            <td>
              <b>$hourly<b><br>
              <a href="time_sliced/figures/$ts_png">
              <img src="time_sliced/figures/$ts_png" width=95%></a> 
              <br> 
              <a href="time_interpolated/figures/$ti_png">
              <img src="time_interpolated/figures/$ti_png" width=95%></a> 
              <br> &nbsp; 
            </td> 
EOF
      let ncol=ncol+1
      if [ $ncol -eq 7 ]; then 
        echo "</tr>" >> $case_dir/index.html
        ncol=1
      fi
      done # while 
      echo "</table>" >> $case_dir/index.html
      cat footer.html >> $case_dir/index.html

    done # case -dir
done # case_year




