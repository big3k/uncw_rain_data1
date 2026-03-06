
# list all the bad hours in v1 (black/blue/red)
./expand_hours.sh Inspection_results.txt  |tail -n+2 |sort > bad_v1_hours.csv

# list all the black/blue hours only, based on v2 data: 

wdir=`pwd`
cd /data1/web/lswg/group/m2m-merge-version4-20251202/step12-hourly-image-v2/2024

ls */*-??00.png | awk -F'[/. -]' '{ print $1 "," $2 "," substr($3,1,2) }' |sort > $wdir/black_blue_hours.csv

cd $wdir

# Join them

comm -12 bad_v1_hours.csv black_blue_hours.csv  > black_blue_bad_hours.csv

./create_html_from_black_blue_bad_hours.sh  > ../compare_v1_v2.html 

