


source define_example_h5.sh

h5dump -d '/All_Data/ATMS-TDR_All/AntennaTemperature' $tatms  |grep '(' |awk -F: '{print $2}' |grep ',' |awk '{printf "%s", $0}'

echo
