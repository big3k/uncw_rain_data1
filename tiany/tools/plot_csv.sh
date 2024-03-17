#! /bin/bash
# 2/11/2024
# - Support multiple input files. Each file will produce a plot in its own panel, and all the plots are arranged
#    in one column. 
# - Output to stdout.  

# 8/6/2018
# Plot a multi-column a csv file. File starts with a header line, and first column is used for x-axis labels. 

# Usage:
# plot_csv <in_file1.csv [in_file2.csv [...]]>

# output will be saved to input_csv_file.html
# Note: to access the .html file, save it to a web dir and access it via the mapped URL. For example: 
# If the output file is: 
#   /data1/fa/test/plot_csv/test_csv.html
# Then it can be accessed by: 
#  http://fa.skyinformatics.com/test/plot_csv/test.csv.html


symbol_scale=4   # controls the size of the node

if [ $# -lt 1 ]; then
   echo "usage:"
   echo "$0 in_file1.csv [in_file2.csv [...]]"
   echo "   each in_file does not have to have the same number of columns"
   exit -1
fi

in_files=("$@")

# check existence of each file
for in_file in "${in_files[@]}"; do
  if [ ! -f $in_file ]; then
    echo "$in_file does not exist ..." 2>&1
    exit -1
  fi
done

# Overall html header
cat <<EOF0
<!DOCTYPE html>
<html lang="en">
<head>
  <title>Visualization of ${in_files[@]}</title>
  <meta charset="utf-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.2/jquery.min.js"></script>
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/dist/echarts.min.js"></script>
  <!-- script src="/echarts-3.3.0/themes/vintage.js"></script -->
  <style>
    /* Set height of the grid so .sidenav can be 100% (adjust as needed) */
    .row.content {height: 550px}

    /* Set gray background color and 100% height */
    .sidenav {
      background-color: #f1f1f1;
      height: 100%;
    }

     .list-group-item:hover {
            background-color: #33b5e5;
            border-color: #33b5e5;
            Xcursor: pointer;
          }

    .panel-default > .panel-heading { background-color: #dfdfdf; }

    li.active > a { color: white; }

    /* On small screens, set height to 'auto' for the grid */
    @media screen and (max-width: 767px) {
      .row.content {height: auto;}
    }
  </style>
</head>
<body>

<div class="container-fluid">
  <div class="row content">
   <div id="plot_panels" class="col-sm-12" style="margin-top: 0">

EOF0

# >>>>>>>>>>>>>>>>>>>>>>>  loop over each file >>>>>>>>>>>>>>>>>>>>>>>>>>
pid=0  #panel id
for in_file in "${in_files[@]}"; do
 let pid=pid+1
 panel="panel$pid" 

# get the header
line1=`head -1 $in_file `
IFS=',' read -r -a headers <<< "$line1"

#legend
legend=`echo $line1 | sed -e "s/^/'/g"  -e "s/,/', '/g" -e "s/$/'/"`

# x-axis data
xdata=`tail -n+2 $in_file | awk -F, '{printf "\47%s\47,", $1}' | sed -e 's/ /\\\n/g'`

# number of columns:
ncols=${#headers[@]}

#get y-data
for i in `seq 2 $ncols`; do
  ydata[$i]=`tail -n+2 $in_file | awk -F, -v f=$i '{printf "%s, ", $f}'`
  #echo --------- i = $i ----------------------------------------------
  #echo ${ydata[$i]}
done

#for header in "${headers[@]}"; do
#    echo "$header"
#done

cat <<EOF

     <!-- 1st panel -->
     <div class="panel panel-primary">
        <div class="panel-heading"> Visualizing $in_file
        </div>
        <div class="panel-body" id="$panel" style="height:600px"></div>
        <div class="panel-footer"> </div>
     </div>

        <script>

(function() {

  var myChart = echarts.init(document.getElementById('$panel'), 'vintage');

  var option = {
    title: {
       text: '$in_file'
    },

    grid: {
          containLabel: false, 
          x: 80,
          y: 80,
          x2: 70,
          y2: 120
    },

    tooltip : {
        show: true,
        trigger: 'axis'
    },

    legend: {
        show: true,
        data: [$legend],
        x: 'right',
        y: 40,
        orient: 'horizontal'
    },

    toolbox: {
        show : true,
        x: 'right',
        y: 'center',
        orient: 'vertical',
        showTitle: false,
        feature : {
            mark : {show: false},
            dataView : {show: false, readOnly: false},
            magicType : {show: true, type: ['line', 'bar', 'stack', 'tiled']},
            restore : {show: false},
            myTool1: {
                show: true,
                title: 'Log/Linear',
                icon: 'path://M 100 180 L 100 400 L 200 400 M 250 250 L 250 400 L 350 400 L 350 250 L 250 250 M 570 400 L 420 400 L 420 250 L 570 250 L 570 550 L 420 550',
                onclick: function (){
                    var type=option.yAxis.type;
                    if ( type === 'value' ) {
                       option.yAxis.type = 'log';
                       option.toolbox.feature.myTool1.icon = 'path://M 100 180 L 100 400 L 200 400 M 250 250 L 250 400 L 350 400 L 350 250 L 250 250 M 570 400 L 420 400 L 420 250 L 570 250 L 570 550 L 420 550 M 100 550 L 570 180 ';
                    } else {
                       option.yAxis.type = 'value';
                       option.toolbox.feature.myTool1.icon = 'path:///M 100 180 L 100 400 L 200 400 M 250 250 L 250 400 L 350 400 L 350 250 L 250 250 M 570 400 L 420 400 L 420 250 L 570 250 L 570 550 L 420 550';
                    }

                    myChart.setOption(option);
                }
            },
            saveAsImage : {show: true}
        }
    },
    dataZoom: {
        show: true,
        start : 0
    },
    xAxis: {
        type: 'category',
        name: '${headers[0]}',
        nameLocation: 'center',
        nameTextStyle: { fontSize: 16 },
        nameGap: 40,
        axisLabel: { align: 'center', rotate: 0, Xinterval: 0, textStyle: {fontSize: 16} },
        data: [ $xdata ]
    },

    yAxis: {
        type: 'value',
        axisLabel: { rotate: 0, Xinterval: 0, textStyle: {fontSize: 16} },
    },

    series : [

EOF

  for i in `seq 2 $ncols`; do
    let j=i-1
     ydata[$i]=`tail -n+2 $in_file | awk -F, -v f=$i '{printf "%s, ", $f}'`

  cat <<EOF1
     { name: '${headers[$j]}',
       type: 'bar',
       itemStyle: {normal: {areaStyle: {type: 'default'}}},
       data: [ ${ydata[$i]} ]
     },
EOF1

done

cat <<EOF2

    ]  // end of series

};     // end of option

                // Load data into the ECharts instance
                myChart.setOption(option);
})();   // end of function

</script>

EOF2

done
# <<<<<<<<<<<<<< end  loop over each file <<<<<<<<<<<<<<<<<<<<<

# tail of html 
cat <<EOF3

    </div>
  </div>
</div>

</body>
</html>

EOF3


