

active=" active"

for hour in {00..23}; do 
   time_str="20230828-${hour}00" 

cat <<EOF
      <div class="item$active">
        <img src="figures/franklin-final-${time_str}.png" alt="$time_str" style="width:80%; margin: auto; height:80%">
      </div>
EOF

active=""

done 
