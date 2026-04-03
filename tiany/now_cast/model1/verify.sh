
./verify_imerg_nowcast.py \
  --forecast-nc forecast_halfhour.nc \
  --imerg-glob "../raw_data/imerg/2025/*20250201*.HDF5" \
  --coarsen-lat 10 \
  --coarsen-lon 10 \
  --out-nc verification_halfhour.nc
``
