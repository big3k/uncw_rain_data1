
# for each event, split them into two parts: the first part contains the first two days of data (96 files), as model input. The 2nd part contains the following 6 hours (12 files) worth of data for validation. 

year=2023

#for event in /data1/tiany/pick_hurranes_events/picked_events/$year/*; do
for event in /data1/tiany/pick_hurranes_events/picked_events/$year/JOVA; do
   event_src_dir=$event
   event_name=$(basename $event) 
   echo Creaating data for $event_name 
   mkdir -p $event_name/input_data
   mkdir -p $event_name/val_data
   in_list=`ls $event_src_dir/*.HDF5 |sort -n |head -96`
   val_list=`ls $event_src_dir/*.HDF5 |sort -n |head -108 |tail -12`
   ln -s $in_list $event_name/input_data/
   ln -s $val_list $event_name/val_data/
   # create forecast and validation scripts
   cat > $event_name/forcast_n_val_0.5.sh <<EOF
#!/usr/bin/env bash
set -euo pipefail

#fx 
/data1/tiany/now_cast/model12_event_training/nowcast_imerg.py forecast \
  --data-glob "input_data/*.HDF5" \
  --coarsen-lat 5 --coarsen-lon 5 \
  --model convlstm \
  --n-days 2 \
  --m-hours 6 \
  --lat-min -40 --lat-max 40 \
  --lon-min -180 --lon-max 180 \
  --ckpt /data1/tiany/now_cast/model12_event_training/runs_0.5/hurricanes2024_convlstm/best.pt \
  --out-nc forecast_0.5.nc

# validation
/data1/tiany/now_cast/model12_event_training/verify_imerg_nowcast.py \
  --forecast-nc forecast_0.5.nc \
  --lat-min -40 --lat-max 40 \
  --lon-min -180 --lon-max 180 \
  --imerg-glob "val_data/*.HDF5" \
  --coarsen-lat 5 \
  --coarsen-lon 5 \
  --out-nc verification_0.5_halfhour.nc

/data1/tiany/now_cast/model12_event_training/plot_verification.py verification_0.5_halfhour.nc

NC=forecast_0.5.nc
BASENAME=\$(basename "\$NC" .nc)

PNGDIR="png_\${BASENAME}"
GIFOUT="\${BASENAME}.gif"

python /data1/tiany/now_cast/model12_event_training/make_nowcast_pngs.py \
  --nc "\$NC" \
  --outdir "\$PNGDIR" \
  --vmax 16

python /data1/tiany/now_cast/model12_event_training/make_gif.py \
  --frames "\$PNGDIR" \
  --out "\$GIFOUT" \
  --fps 4

EOF

chmod u+x $event_name/forcast_n_val_0.5.sh

done


