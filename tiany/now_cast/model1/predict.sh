# Forecast using the last N days from files on disk
./nowcast_imerg.py forecast \
  --data-glob "../raw_data/imerg/2025/*202501*.HDF5" \
  --engine h5netcdf --group Grid \
  --coarsen-lat 10 --coarsen-lon 10 \
  --n-days 2 \
  --m-hours 6 \
  --ckpt runs/imerg_halfhour/best.pt \
  --out-png forecast_halfhour.png \
  --out-nc forecast_halfhour.nc

