#! /bin/bash 

[ -f ./config ] || { echo "config does not exist. exit."; exit; }
set -a; . ./config; set +a   # set up user-defined variables. 

export top_dir=`pwd`

./code/extract_gfs.sh



echo $start_day
echo $top_dir

