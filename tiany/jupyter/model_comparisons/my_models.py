
import os
import glob
import scipy.io
import numpy as np
import tensorflow as tf
from tensorflow.keras import layers, models, losses
import matplotlib.pyplot as plt
from tensorflow.keras.layers import Input, Conv2D, MaxPooling2D, UpSampling2D
from tensorflow.keras.models import Model
from scipy.stats import gaussian_kde
from sklearn.metrics import root_mean_squared_error

def ssim_loss(y_true, y_pred):
    return 1 - tf.reduce_mean(tf.image.ssim(y_true, y_pred, max_val=1.0))

def model_validation(val_ds, scale, model):
   # validation
   predictions=np.empty([0, 96, 96, 1])
   low_res=np.empty([0, 96, 96, 1])
   high_res=np.empty([0, 96, 96, 1])
   for low_res1, high_res1 in val_ds:   # test all val_ds samples
      prediction = model.predict(low_res1)
      predictions=np.concatenate((predictions, prediction))
      low_res=np.concatenate((low_res, low_res1))
      high_res=np.concatenate((high_res, high_res1))

   # Assuming prediction and high_res have shape (16, height, width, 1) in each batch
   # Flatten them to 1D arrays
   pred_flat = predictions.flatten()*scale  # prediction, i.e., enhanced SSMIS
   gt_flat = high_res.flatten()*scale   # groud truth, i.e., GMI
   lr_flat = low_res.flatten()*scale

   bias_hr = np.mean(pred_flat) - np.mean(gt_flat)
   bias_lr = np.mean(lr_flat) - np.mean(gt_flat)

   rms_hr = root_mean_squared_error(gt_flat, pred_flat)
   rms_lr = root_mean_squared_error(gt_flat, lr_flat)

   cor_hr=np.corrcoef(gt_flat, pred_flat)[0, 1]
   cor_lr=np.corrcoef(gt_flat, lr_flat)[0, 1]

   return [bias_lr, rms_lr, cor_lr], [bias_hr, rms_hr, cor_hr]

# plot training loss and mse metrics, given history  
def plot_history(history, epochs, fig_name):

   mse = history.history['mse']
   loss = history.history['loss']

   epochs_range = range(epochs)
   plt.figure(figsize=(8, 8))
   plt.subplot(1, 2, 1)
   plt.plot(epochs_range, mse, label='Training MSE')
   plt.title('Training MSE')

   plt.subplot(1, 2, 2)
   plt.plot(epochs_range, loss, label='Training Loss')
   plt.title('Training  Loss')
   #plt.show()
   plt.savefig(fig_name)

   return


def model_1a(train_ds, val_ds, scale, epochs=150): 

   # Encoder
   input_img = Input(shape=(96, 96, 1))
   x = Conv2D(32, (3, 3), activation="relu", padding="same")(input_img)
   x = MaxPooling2D((2, 2), padding="same")(x)
   #x = Conv2D(64, (3, 3), activation="relu", padding="same")(x)
   #x = MaxPooling2D((2, 2), padding="same")(x)

   # Bottleneck
   x = Conv2D(128, (3, 3), activation="relu", padding="same")(x)

   # Decoder
   #x = UpSampling2D((2, 2))(x)
   #x = Conv2D(64, (3, 3), activation="relu", padding="same")(x)
   x = UpSampling2D((2, 2))(x)
   x = Conv2D(32, (3, 3), activation="relu", padding="same")(x)

   output_img = Conv2D(1, (3, 3), activation="sigmoid", padding="same")(x)

   # Compile & Train
   autoencoder = Model(input_img, output_img)
   #autoencoder.compile(optimizer="adam", loss="mae") # not working for epochs=4
   #autoencoder.compile(optimizer="adam", loss="mse") # not working for epochs=4
   autoencoder.compile(optimizer="adam", loss=ssim_loss, metrics=['mse']) # works for epochs=4, 10, 100

   # autoencoder.summary()
   history=autoencoder.fit(train_ds, epochs=epochs)

   #autoencoder.save('autoencoder_'+ str(epochs) + '_epochs.keras')
   #tf.keras.utils.plot_model(autoencoder, show_shapes=True, show_dtype=True, show_layer_names=True)
   plot_history(history, epochs, "model_1a_history.png")

   # validation 
   old_metrics, new_metrics =  model_validation(val_ds, scale, autoencoder)

   return old_metrics, new_metrics 


def model_2a(train_ds, val_ds, scale, epochs=150): 

 class Denoise(Model):
  def __init__(self):
    super(Denoise, self).__init__()
    self.encoder = tf.keras.Sequential([
      layers.Input(shape=(96, 96, 1)),
      layers.Conv2D(16, (3, 3), activation='relu', padding='same', strides=2),
      layers.Conv2D(8, (3, 3), activation='relu', padding='same', strides=2)])

    self.decoder = tf.keras.Sequential([
      layers.Conv2DTranspose(8, kernel_size=3, strides=2, activation='relu', padding='same'),
      layers.Conv2DTranspose(16, kernel_size=3, strides=2, activation='relu', padding='same'),
      layers.Conv2D(1, kernel_size=(3, 3), activation='sigmoid', padding='same')])

  def call(self, x):
    encoded = self.encoder(x)
    decoded = self.decoder(encoded)
    return decoded

 autoencoder = Denoise()
 autoencoder.compile(optimizer="adam", loss=ssim_loss, metrics=['mse'])
 history=autoencoder.fit(train_ds, epochs=epochs) 

 plot_history(history, epochs, "model_2a_history.png")

 # validation 
 old_metrics, new_metrics =  model_validation(val_ds, scale, autoencoder)

 return old_metrics, new_metrics 

  


