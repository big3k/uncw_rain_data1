#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os
import glob
import scipy.io
import numpy as np
import tensorflow as tf
from tensorflow.keras import layers, models
import matplotlib.pyplot as plt
from tensorflow.keras.layers import Input, Conv2D, MaxPooling2D, UpSampling2D
from tensorflow.keras.models import Model
import my_models

import load_mat_images # our own module to read data

# Read the data in as tf Dataset
# max of height and width is 101.
print("Reading input data ...") 
dataset, scale=load_mat_images.load_dataset(fullset=True, height=96, width=96)  # False: only take 100 images for quick tests

seed=123  # fix for reproducibility
tf.random.set_seed(seed)
np.random.seed(seed=seed)

total_size=len(dataset) # total batches 
dataset = dataset.shuffle(dataset.cardinality(), seed=seed)
print("len(dataset): ", len(dataset))
# split into train and val: 
train_ratio = 0.8
train_size = int(train_ratio * total_size)
train_ds = dataset.take(train_size)
val_ds = dataset.skip(train_size)
print("len(rain_ds): ", len(train_ds), " len(val_ds): ", len(val_ds)) 

# header 
print(f"==>Model, pre/post-training, Bias, RMSE, Corr") 

epochs=150
# each metrics is a list [Bias, RMSE, Corr]
old_metrics, new_metrics = my_models.model_1a( train_ds, val_ds, scale, epochs=epochs ) 
print(f"==>model_1a, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>model_1a, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 

old_metrics, new_metrics = my_models.model_2a( train_ds, val_ds, scale, epochs=epochs ) 
print(f"==>model_1a, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>model_1a, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 



