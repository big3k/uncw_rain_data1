

cat <<EOF
<table class="table table-striped">
    <thead>
      <tr>
        <th>Case</th>
        <th>m2m_cf</th>
        <th>h2h_cf</th>
        <th>m2m_bias(%)</th>
        <th>h2h_bias(%)</th>
        <th>m2m_rmse</th>
        <th>h2h_rmse</th>
      </tr>
    </thead>
    <tbody>
EOF

tail -n+2 summary.csv | awk -F, '
   { printf "<tr><td><a href=\"N19/%s/\">%s</a></td><td>%4.2f</td><td>%4.2f</td><td>%4.2f</td><td>%4.2f</td><td>%4.2f</td><td>%4.2f</td></tr>\n", $1, $1, $3, $4, $5*100, $6*100, $7, $8}' 

echo "</tbody></table>"

#case_path,mat_name,m2m_cf,h2h_cf,m2m_bias,h2h_bias,m2m_rmse,h2h_rmse
#201407/case_0101,sate_select_case_002.mat,0.308102940365495,0.3033590
