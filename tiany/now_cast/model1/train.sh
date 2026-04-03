
  # Train on a day’s worth of half-hourly files and coarsen to 1 degree
./nowcast_imerg.py train \
  --data-glob "../raw_data/imerg/2025/*202501*.HDF5" \
  --engine h5netcdf --group Grid \
  --coarsen-lat 10 --coarsen-lon 10 \
  --n-days 2 \
  --m-hours 6 \
  --epochs 6 \
  --batch-size 2 \
  --outdir runs/imerg_halfhour

exit
# Forecast using the last N days from files on disk
./nowcast_imerg.py forecast \
  --data-glob "imerg/2025/*.HDF5" \
  --engine h5netcdf --group Grid \
  --coarsen-lat 10 --coarsen-lon 10 \
  --n-days 2 \
  --m-hours 6 \
  --ckpt runs/imerg_halfhour/best.pt \
  --out-png forecast_halfhour.png \
  --out-nc forecast_halfhour.nc

