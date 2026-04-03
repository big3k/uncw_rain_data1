
import xarray as xr
import matplotlib.pyplot as plt

ds = xr.open_dataset("verification.nc")

plt.figure(figsize=(6,4))
plt.plot(ds.lead_time, ds.rmse, label="RMSE")
plt.plot(ds.lead_time, ds.mae, label="MAE")
plt.xlabel("Lead time (h)")
plt.ylabel("mm/hr")
plt.legend()
plt.title("Half-hourly Nowcast Skill")
plt.grid(True)
plt.show()
