
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
from scipy.io import savemat 

def ssim_loss(y_true, y_pred):
    return 1 - tf.reduce_mean(tf.image.ssim(y_true, y_pred, max_val=1.0))

def model_validation(val_ds, scale, model, m_name):
   # validation
   predictions=np.empty([0, 96, 96, 1])
   low_res=np.empty([0, 96, 96, 1])
   high_res=np.empty([0, 96, 96, 1])
   for low_res1, high_res1 in val_ds:   # test all val_ds samples
      prediction = model.predict(low_res1)
      predictions=np.concatenate((predictions, prediction))
      low_res=np.concatenate((low_res, low_res1))
      high_res=np.concatenate((high_res, high_res1))

   # save to matlab file 
   # Your arrays: predictions, low_res, high_res
   data_dict = {
    'enhanced_ssmis': predictions*scale,
    'original_ssmis': low_res*scale,
    'gmi': high_res*scale
   }

   savemat(m_name + '_valdata.mat', data_dict)

   # sample plot
   num_samples = 6
   fig, axes = plt.subplots(3, num_samples, figsize=(num_samples * 2, 6))

   for i in range(num_samples):
    j=np.random.randint(1, high=len(low_res)-1) # select random sample
    # Low-quality input
    axes[0, i].imshow(low_res[j].squeeze(), cmap="gist_ncar", vmin=0, vmax=.35)
    axes[0, i].set_title("SSMIS")
    axes[0, i].axis("off")

    # Ground truth high-quality
    axes[1, i].imshow(high_res[j].squeeze(), cmap="gist_ncar", vmin=0, vmax=.35)
    axes[1, i].set_title("GMI")
    axes[1, i].axis("off")

    # Model prediction
    axes[2, i].imshow(predictions[j].squeeze(), cmap="gist_ncar", vmin=0, vmax=.35)
    axes[2, i].set_title("Enhanced SSMIS")
    axes[2, i].axis("off")

   plt.tight_layout()
   #plt.show()
   plt.savefig(m_name + '_samples.png')

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

  loss = history.history['loss']
  val_loss = history.history['val_loss']

  mse = history.history['mse']
  val_mse = history.history['val_mse']

  epochs_range = range(epochs)

  plt.figure(figsize=(12, 6))
  plt.subplot(1, 2, 1)
  plt.plot(epochs_range, loss, label='Training Loss')
  plt.plot(epochs_range, val_loss, label='Validation Loss')
  plt.legend(loc='upper right')
  plt.title('Training Loss')

  plt.subplot(1, 2, 2)
  plt.plot(epochs_range, mse, label='Training MSE')
  plt.plot(epochs_range, val_mse, label='Validation MSE')
  plt.legend(loc='upper right')
  plt.title('Training MSE')
  plt.savefig(fig_name)

  return

# Autoencoder model with MaxPooling for dimension reduction 
def model_1(train_ds, val_ds, scale, epochs=150, loss_f='mse', act_f='relu', m_name='model_1a'): 

   # Encoder
   input_img = Input(shape=(96, 96, 1))
   x = Conv2D(32, (3, 3), activation=act_f, padding="same")(input_img)
   x = MaxPooling2D((2, 2), padding="same")(x)
   #x = Conv2D(64, (3, 3), activation="relu", padding="same")(x)
   #x = MaxPooling2D((2, 2), padding="same")(x)

   # Bottleneck
   x = Conv2D(128, (3, 3), activation=act_f, padding="same")(x)

   # Decoder
   #x = UpSampling2D((2, 2))(x)
   #x = Conv2D(64, (3, 3), activation="relu", padding="same")(x)
   x = UpSampling2D((2, 2))(x)
   x = Conv2D(32, (3, 3), activation=act_f, padding="same")(x)

   output_img = Conv2D(1, (3, 3), activation="sigmoid", padding="same")(x)

   # Compile & Train
   autoencoder = Model(input_img, output_img)
   #autoencoder.compile(optimizer="adam", loss="mae") # not working for epochs=4
   #autoencoder.compile(optimizer="adam", loss="mse") # not working for epochs=4
   autoencoder.compile(optimizer="adam", loss=loss_f, metrics=['mse']) # works for epochs=4, 10, 100

   # autoencoder.summary()
   history=autoencoder.fit(train_ds, validation_data=val_ds, epochs=epochs)

   #autoencoder.save('autoencoder_'+ str(epochs) + '_epochs.keras')
   #tf.keras.utils.plot_model(autoencoder, show_shapes=True, show_dtype=True, show_layer_names=True)
   plot_history(history, epochs, m_name + "_history.png")

   # validation 
   old_metrics, new_metrics =  model_validation(val_ds, scale, autoencoder, m_name)

   return old_metrics, new_metrics 

# Autoencoder model with stride > 1 for dimension reduction 
def model_2(train_ds, val_ds, scale, epochs=150, loss_f='mse', act_f='relu', m_name='model_2a'): 

 class Denoise(Model):
  def __init__(self):
    super(Denoise, self).__init__()
    self.encoder = tf.keras.Sequential([
      layers.Input(shape=(96, 96, 1)),
      layers.Conv2D(16, (3, 3), activation=act_f, padding='same', strides=2),
      layers.Conv2D(8, (3, 3), activation=act_f, padding='same', strides=2)])

    self.decoder = tf.keras.Sequential([
      layers.Conv2DTranspose(8, kernel_size=3, strides=2, activation=act_f, padding='same'),
      layers.Conv2DTranspose(16, kernel_size=3, strides=2, activation=act_f, padding='same'),
      layers.Conv2D(1, kernel_size=(3, 3), activation='sigmoid', padding='same')])

  def call(self, x):
    encoded = self.encoder(x)
    decoded = self.decoder(encoded)
    return decoded

 autoencoder = Denoise()
 autoencoder.compile(optimizer="adam", loss=loss_f, metrics=['mse'])
 history=autoencoder.fit(train_ds, validation_data=val_ds, epochs=epochs) 

 plot_history(history, epochs, m_name + "_history.png")

 # validation 
 old_metrics, new_metrics =  model_validation(val_ds, scale, autoencoder, m_name)

 return old_metrics, new_metrics 

# Conventional SRCNN 
def model_3(train_ds, val_ds, scale, epochs=150, loss_f='mse', act_f='relu', m_name='model_3a'): 

   # CNN, 3 layers 
   input_img = Input(shape=(96, 96, 1))
   x = Conv2D(64, (9, 9), activation=act_f, padding="same")(input_img)
   x = Conv2D(32, (1, 1), activation=act_f, padding="same")(x) 
   output_img = Conv2D(1, (5, 5), activation=act_f, padding="same")(x)

   # Compile & Train
   srcnn = Model(input_img, output_img)
   srcnn.compile(optimizer="adam", loss=loss_f, metrics=['mse']) 

   # srcnn.summary()
   history=srcnn.fit(train_ds, validation_data=val_ds, epochs=epochs)

   #srcnn.save('srcnn_'+ str(epochs) + '_epochs.keras')
   #tf.keras.utils.plot_model(srcnn, show_shapes=True, show_dtype=True, show_layer_names=True)
   plot_history(history, epochs, m_name + "_history.png")

   # validation 
   old_metrics, new_metrics =  model_validation(val_ds, scale, srcnn, m_name)

   return old_metrics, new_metrics 



