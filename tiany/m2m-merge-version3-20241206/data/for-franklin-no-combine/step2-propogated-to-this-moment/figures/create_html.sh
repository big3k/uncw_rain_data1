
# organize the plots: 

cat <<HEAD
<!DOCTYPE html>
<html lang="en">
<head>
  <title>Optimization of Microwave Retrievals on 2D</title>
  <meta charset="utf-8">
  <meta http-equiv="Cache-Control" content="no-store" />
  <meta http-equiv="Cache-Control" content="no-cache, no-store, must-revalidate" />
  <meta http-equiv="Pragma" content="no-cache" />
  <meta http-equiv="Expires" content="0" />
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
  <link rel="stylesheet" href="https://cdn.datatables.net/1.13.1/css/dataTables.bootstrap.min.css">
  <link rel="stylesheet" href="https://cdn.datatables.net/datetime/1.2.0/css/dataTables.dateTime.min.css">
  <link rel="stylesheet" href="https://cdn.datatables.net/searchbuilder/1.4.0/css/searchBuilder.bootstrap.min.css">

  <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
  <script src="https://cdn.datatables.net/1.13.1/js/jquery.dataTables.min.js"></script>
  <script src="https://cdn.datatables.net/1.13.1/js/dataTables.bootstrap.min.js"></script>
  <script src="https://cdn.datatables.net/datetime/1.2.0/js/dataTables.dateTime.min.js"></script>
  <script src="https://cdn.datatables.net/searchbuilder/1.4.0/js/dataTables.searchBuilder.min.js"></script>

  <style>
    /* Remove the navbar's default rounded borders and increase the bottom margin */ 
    .navbar {
      margin-bottom: 20px;
      border-radius: 0;
      background-color: #424242;
    }
  
    .nav > li > a { 
     color: #5bc0de; 
    }

   .text-left {
      text-align: left !important;
    }

    .panel-primary > .panel-heading { 
     X-color: #000; 
     X-background-color: #999; 
    }
    
    /* customize the jumbotron's default margins */ 
     .jumbotron {
      background-image: url("images/cloud.jpg");
      background-size: cover;
      padding: 10px; 
      padding-left: 10px; 
      margin-top: 0;
      margin-bottom: 10;
    }

    .btn-primary { 
          background-color: #999; 
    }

    /* Add a gray background color and some padding to the footer */
    footer {
      background-color: #f2f2f2;
      padding: 25px;
    }
  </style>
</head>
<body>
<div class="container">
HEAD


# anchors
ls event.20230828*.mat.png |sort |while read line; do 
  echo "<h3>$line</h3><a href=\"$line\"><img src="$line" width=20%></a><br>" 
  matf=`echo $line | sed -e 's/.png$//'`
  # time-matched files 
  for tfile in ${matf}-*.png; do 
    echo "<a href=\"$tfile\"><img src="$tfile" width=10%></a>" 
  done
  echo "<br><br>" 
done 


cat <<TAIL
<br><br>
</div>
</body>
</html>
TAIL
