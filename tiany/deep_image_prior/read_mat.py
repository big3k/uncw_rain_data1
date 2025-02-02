#! /data1/tiany/miniconda3/bin/python
import scipy.io as spio
import numpy as np

def get_storm(): 
    mfile='/data1/tiany/m2m-merge-version4/output/2023/FRANKLIN/time_interpolated/20230829-0800-moved-from-20230829-0600-event.20230829-0649.lon-n071.lat-p30-GCOMW1.mat-merged.mat.mat'
# image of this file:
# https://lswg.uncw.edu/group/m2m-merge-version4/output/2023/FRANKLIN/time_interpolated/new_figures/20230829-0800-moved-from-20230829-0600-event.20230829-0649.lon-n071.lat-p30-GCOMW1.mat-merged.mat.mat.png

    mat=spio.loadmat(mfile)
    moved=mat.get('moved')

    rate=moved[0][0][0] 
    np_rate=np.flipud(np.array(rate)) # flip upside down 

    zm_rate=np_rate[100:200, 100:200]  # zoom to storm center, 100x100 

    return zm_rate
# plt.imshow(zm_rate, interpolation='none')
# plt.show()



