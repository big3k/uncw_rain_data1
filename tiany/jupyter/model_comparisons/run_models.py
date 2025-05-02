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
train_ds, val_ds, scale=load_mat_images.load_dataset(fullset=True, height=96, width=96)  # False: only take 100 images for quick tests

epochs=300

seed=123  # fix for reproducibility
tf.random.set_seed(seed)
np.random.seed(seed=seed)

'''
total_size=len(dataset) # total batches 
dataset = dataset.shuffle(dataset.cardinality(), seed=seed)
print("len(dataset): ", len(dataset))
# split into train and val: 
train_ratio = 0.8
train_size = int(train_ratio * total_size)
train_ds = dataset.take(train_size)
val_ds = dataset.skip(train_size)
'''

print("len(rain_ds): ", len(train_ds), " len(val_ds): ", len(val_ds)) 

# header 
print(f"==>Model, loss_f, act_f, pre/post-training, Bias, RMSE, Corr") 

#== ax ======================

# each metrics is a list [Bias, RMSE, Corr]
loss_f=my_models.ssim_loss # a 
#loss_f='mse'  # b 
#loss_f='mae'  # c 
act_f='relu'  # x 
#act_f='tanh'  # y 


# --- model 1 and 2 ---------------------
m_name="model_1ax" 
old_metrics, new_metrics = my_models.model_1( train_ds, val_ds, scale, epochs=epochs, 
                           loss_f=loss_f, act_f=act_f, m_name=m_name) 
print(f"==>{m_name}, ssim_loss, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, ssim_loss, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 

m_name="model_2ax" 
old_metrics, new_metrics = my_models.model_2( train_ds, val_ds, scale, epochs=epochs, 
                           loss_f=loss_f, act_f=act_f, m_name=m_name) 
print(f"==>{m_name}, ssim_loss, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, ssim_loss, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 

m_name="model_3ax" 
old_metrics, new_metrics = my_models.model_3( train_ds, val_ds, scale, epochs=epochs, 
                           loss_f=loss_f, act_f=act_f, m_name=m_name) 
print(f"==>{m_name}, ssim_loss, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, ssim_loss, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 


# ---------------------------------------

#== bx ======================

#loss_f=my_models.ssim_loss # a
loss_f='mse'  # b
#loss_f='mae'  # c
act_f='relu'  # x
#act_f='tanh'  # y 

# --- model 1 and 2 ---------------------
m_name="model_1bx"
old_metrics, new_metrics = my_models.model_1( train_ds, val_ds, scale, epochs=epochs,
                           loss_f=loss_f, act_f=act_f, m_name=m_name)
print(f"==>{m_name}, {loss_f}, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, {loss_f}, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}")

m_name="model_2bx"
old_metrics, new_metrics = my_models.model_2( train_ds, val_ds, scale, epochs=epochs,
                           loss_f=loss_f, act_f=act_f, m_name=m_name)
print(f"==>{m_name}, {loss_f}, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, {loss_f}, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}")

m_name="model_3bx" 
old_metrics, new_metrics = my_models.model_3( train_ds, val_ds, scale, epochs=epochs, 
                           loss_f=loss_f, act_f=act_f, m_name=m_name) 
print(f"==>{m_name}, ssim_loss, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, ssim_loss, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 
# ---------------------------------------

#== cx ======================

#loss_f=my_models.ssim_loss # a
#loss_f='mse'  # b
loss_f='mae'  # c
act_f='relu'  # x
#act_f='tanh'  # y 

# --- model 1 and 2 ---------------------
m_name="model_1cx"
old_metrics, new_metrics = my_models.model_1( train_ds, val_ds, scale, epochs=epochs,
                           loss_f=loss_f, act_f=act_f, m_name=m_name)
print(f"==>{m_name}, {loss_f}, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, {loss_f}, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}")

m_name="model_2cx"
old_metrics, new_metrics = my_models.model_2( train_ds, val_ds, scale, epochs=epochs,
                           loss_f=loss_f, act_f=act_f, m_name=m_name)
print(f"==>{m_name}, {loss_f}, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, {loss_f}, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}")

m_name="model_3cx" 
old_metrics, new_metrics = my_models.model_3( train_ds, val_ds, scale, epochs=epochs, 
                           loss_f=loss_f, act_f=act_f, m_name=m_name) 
print(f"==>{m_name}, ssim_loss, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, ssim_loss, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 
# ---------------------------------------


#== ay ======================

# each metrics is a list [Bias, RMSE, Corr]
loss_f=my_models.ssim_loss # a 
#loss_f='mse'  # b 
#loss_f='mae'  # c 
#act_f='relu'  # x 
act_f='tanh'  # y 


# --- model 1 and 2 ---------------------
m_name="model_1ay" 
old_metrics, new_metrics = my_models.model_1( train_ds, val_ds, scale, epochs=epochs, 
                           loss_f=loss_f, act_f=act_f, m_name=m_name) 
print(f"==>{m_name}, ssim_loss, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, ssim_loss, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 

m_name="model_2ay" 
old_metrics, new_metrics = my_models.model_2( train_ds, val_ds, scale, epochs=epochs, 
                           loss_f=loss_f, act_f=act_f, m_name=m_name) 
print(f"==>{m_name}, ssim_loss, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, ssim_loss, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 

m_name="model_3ay" 
old_metrics, new_metrics = my_models.model_3( train_ds, val_ds, scale, epochs=epochs, 
                           loss_f=loss_f, act_f=act_f, m_name=m_name) 
print(f"==>{m_name}, ssim_loss, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, ssim_loss, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 
# ---------------------------------------

#== by ======================

#loss_f=my_models.ssim_loss # a
loss_f='mse'  # b
#loss_f='mae'  # c
#act_f='relu'  # x
act_f='tanh'  # y 

# --- model 1 and 2 ---------------------
m_name="model_1by"
old_metrics, new_metrics = my_models.model_1( train_ds, val_ds, scale, epochs=epochs,
                           loss_f=loss_f, act_f=act_f, m_name=m_name)
print(f"==>{m_name}, {loss_f}, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, {loss_f}, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}")

m_name="model_2by"
old_metrics, new_metrics = my_models.model_2( train_ds, val_ds, scale, epochs=epochs,
                           loss_f=loss_f, act_f=act_f, m_name=m_name)
print(f"==>{m_name}, {loss_f}, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, {loss_f}, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}")

m_name="model_3by" 
old_metrics, new_metrics = my_models.model_3( train_ds, val_ds, scale, epochs=epochs, 
                           loss_f=loss_f, act_f=act_f, m_name=m_name) 
print(f"==>{m_name}, ssim_loss, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, ssim_loss, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 
# ---------------------------------------

#== cy ======================

#loss_f=my_models.ssim_loss # a
#loss_f='mse'  # b
loss_f='mae'  # c
#act_f='relu'  # x
act_f='tanh'  # y 

# --- model 1 and 2 ---------------------
m_name="model_1cy"
old_metrics, new_metrics = my_models.model_1( train_ds, val_ds, scale, epochs=epochs,
                           loss_f=loss_f, act_f=act_f, m_name=m_name)
print(f"==>{m_name}, {loss_f}, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, {loss_f}, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}")

m_name="model_2cy"
old_metrics, new_metrics = my_models.model_2( train_ds, val_ds, scale, epochs=epochs,
                           loss_f=loss_f, act_f=act_f, m_name=m_name)
print(f"==>{m_name}, {loss_f}, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, {loss_f}, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}")

m_name="model_3cy" 
old_metrics, new_metrics = my_models.model_3( train_ds, val_ds, scale, epochs=epochs, 
                           loss_f=loss_f, act_f=act_f, m_name=m_name) 
print(f"==>{m_name}, ssim_loss, {act_f}, pre, {old_metrics[0]:.3f},{old_metrics[1]:.3f}, {old_metrics[2]:.3f}")
print(f"==>{m_name}, ssim_loss, {act_f}, post,{new_metrics[0]:.3f}, {new_metrics[1]:.3f}, {new_metrics[2]:.3f}") 
# ---------------------------------------




