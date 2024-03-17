#! /bin/bash

# Usage:
# plot_scatter.sh in_file1.csv [in_file2.csv [...]]

# output will be to stdout 
#
#
# It visualize each input csv file as scatter plot, overlaying all the input files.
# The number of columns is assumed to be the same for all the input files.
# The csv file assumes a header line, and can have one to four columns.
# Depending on the number of columns, it will generate the following types of scatter plots:
#
# Single-column:
#-----------------------------------------------------------------
#  size
#  3
#  15
#  ...

# It will generate a "star map", randomly placed circles on the [0-100]x[0-100] plane, and
#   the 'size' values are the sizes of the circles.

# Two-column:
#-----------------------------------------------------------------
#  x, y
#  3,2
#  15,10
#  ...

# It will generate a scatter plot with fixed symbol size, with the location of each symbol
#  defined by (col1, col2) values.

# Three-column:
#-----------------------------------------------------------------
#  x, y, size
#  2, 3, 10
#  3, 4, 2
#  ...

# It will generate a scatter plot similar to the two-column type, but with the sizes of
# the circles given in the 3rd column.

# Four-column:
#-----------------------------------------------------------------
#  x, y, size, label
#  2, 3, 10, abc
#  3, 4, 2,  xyz3
#  ...

# It will generate a scatter plot similar to the two-column type, but with the sizes of
# the circles given in the 3rd column, and each cycle is labeled by the 4th column.

#
# The user needs to scale the 'size' value for proper display.
#

declare -A data

symbol_size=6  # default symbol size

if [ $# -lt 1 ]; then
   echo "usage:"
   echo "$0 in_file1.csv [in_file2.csv [...]]"
   echo "   each in_file has to have the same number of columns"
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

in_file1=${in_files[0]} # first input file
#out_file=`basename $in_file1`_etc.html
out_file=/dev/stdout 

# get the header
line1=`head -1 $in_file1 |tr -d '\r' `
IFS=',' read -r -a headers <<< "$line1"

#legend
legend=`echo ${in_files[@]}| sed -e "s/^/'/g"  -e "s/ /', '/g" -e "s/$/'/"`

# number of columns:
ncols=${#headers[@]}

if [ $ncols -eq 1 ]; then  # 1-column format
   for in_file in "${in_files[@]}"; do
     data[$in_file]=`tail -n+2 $in_file | tr -d '\r' | awk -vs=$RANDOM 'BEGIN{ srand(s) } {printf "[%5.1f,%5.1f,%d,\x27\x27], ", rand()*100, rand()*100, int($1)}'`
   done
   headers=('x', 'y', ${headers[0]})
fi

if [ $ncols -eq 2 ]; then  # 2-column format
   for in_file in "${in_files[@]}"; do
     data[$in_file]=`tail -n+2 $in_file | tr -d '\r' | awk -F, -vsize=$symbol_size '{printf "[%s,%s,%s,\x27\x27], ", $1, $2, int(size)}'`
   done
fi

if [ $ncols -eq 3 ]; then  # 3-column format
   for in_file in "${in_files[@]}"; do
     data[$in_file]=`tail -n+2 $in_file | tr -d '\r' | awk -F, '{printf "[%s,%s,%s,\x27\x27], ", $1, $2, int($3)}'`
   done
fi

if [ $ncols -ge 4 ]; then  # 4-column format. Columns over 4th will be ignored.
   for in_file in "${in_files[@]}"; do
     data[$in_file]=`tail -n+2 $in_file | tr -d '\r' | awk -F, '{printf "[%s,%s,%s,\x27%s\x27], ", $1, $2, int($3), $4}'`
   done
fi

#for header in "${headers[@]}"; do
#    echo "$header"
#done

cat > $out_file <<EOF

<!DOCTYPE html>
<html lang="en">
<head>
  <title>Data Visualization</title>
  <meta charset="utf-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.2/jquery.min.js"></script>
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/dist/echarts.min.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/theme/macarons.js"></script>
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


     <!-- 1st panel -->
     <div class="panel panel-primary">
        <div class="panel-heading"> Visualizing ${in_files[@]}
        </div>
        <div class="panel-body" id="links" style="height:700px"></div>
        <div class="panel-footer"> </div>
     </div>



<!-- Start of Javascript -->
        <script>

(function() {

  var myChart = echarts.init(document.getElementById('links'), 'macarons');

  var option = {
    title: {
       text: '${in_files[@]}'
    },

    grid: {
          x: 80,
          y: 80,
          x2: 70,
          y2: 120
    },

    tooltip : {
        show: true,
        trigger: 'item'
    },

    legend: {
        show: true,
        data: [$legend],
        x: 'right',
        y: 40,
        orient: 'horizontal'
    },

    toolbox: {
        show : false,
        x: 'right',
        y: 'center',
        orient: 'vertical',
        showTitle: false,
        feature : {
            mark : {show: false},
            dataView : {show: false, readOnly: false},
            magicType : {show: false, type: ['line', 'bar', 'stack', 'tiled']},
            restore : {show: false},
            myTool1: {
                show: true,
                title: 'Log/Linear',
                icon: 'path://M 100 180 L 100 400 L 200 400 M 250 250 L 250 400 L 350 400 L 350 250 L 250 250 M 570 400 L 420 400 L 420 250 L 570 250 L 570 550 L 420 550',
                onclick: function (){
                    var type=option.xAxis.type;
                    if ( type === 'value' ) {
                       option.xAxis.type = 'log';
                       option.toolbox.feature.myTool1.icon = 'path://M 100 180 L 100 400 L 200 400 M 250 250 L 250 400 L 350 400 L 350 250 L 250 250 M 570 400 L 420 400 L 420 250 L 570 250 L 570 550 L 420 550 M 100 550 L 570 180 ';
                    } else {
                       option.xAxis.type = 'value';
                       option.toolbox.feature.myTool1.icon = 'path:///M 100 180 L 100 400 L 200 400 M 250 250 L 250 400 L 350 400 L 350 250 L 250 250 M 570 400 L 420 400 L 420 250 L 570 250 L 570 550 L 420 550';
                    }

                    myChart.setOption(option);
                }
            },
            saveAsImage : {show: true}
        }
    },
dataZoom: [
    {
      show: true,
      xAxisIndex: [0]
    },
    {
      left: 'left',
      show: true,
      yAxisIndex: [0],
    }
  ],
    xAxis: {
        type: 'value',
        name: '${headers[0]}',
        splitLine: { show: false },
        splitArea: { show: false },
        axisLabel: { rotate: 0, Xinterval: 0, textStyle: {XfontSize: 20} }
    },

    yAxis: {
        type: 'value',
        name: '${headers[1]}',
        splitLine: { show: false },
        splitArea: { show: false },
    },

    series : [

EOF

 for in_file in "${in_files[@]}"; do
    cat >> $out_file <<EOF1
     { name: '$in_file',
       type: 'scatter',
       encode: { tooltip: [, 3] },   //show label when mouse over
       itemStyle: {normal: {areaStyle: {type: 'default'}, Xcolor:'#9b59b6'}},
       symbolSize: function(data) { return data[2]; },
       label: { show: false, formatter: '{@[3]}', color: '#000'},
       data: [ ${data[$in_file]} ]
     },
EOF1

done

cat >> $out_file <<EOF2

    ]  // end of series

};     // end of option

                // Load data into the ECharts instance
                myChart.setOption(option);
})();   // end of function

</script>
<!-- End of Javascript -->




    </div>
  </div>
</div>

</body>
</html>


EOF2

