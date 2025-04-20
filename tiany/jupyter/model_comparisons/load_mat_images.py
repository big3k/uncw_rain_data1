# Keep input data as np.arrary (don't convert to images), and don't scale to 0-1

import os
import glob
import scipy.io
import numpy as np
import tensorflow as tf
from PIL import Image
from tensorflow.keras.preprocessing.image import array_to_img
from sklearn.model_selection import train_test_split

# Sample file: 
#  101x101: 
# load('/data1/youy/tropics-hurricane/figure/cat1-cat5-tropics-gpm-update-filter-super-reso-v2/ssmis//2179.mat')

# height and width can be <= 101. 
def load_mat_images(file_pattern, variable_name='rain1', height=101, width=101):
    """
    Load all .mat files that fit the file_pattern as images. 
     then split into training and test sets.
    
    Parameters:
        file_pattern (str): pattern string for glob() to find all the .mat files.
          e.g., '/data1/tiany/m2m-merge-version4/output/2019/*/time_interpolated/*.mat'
        variable_name (str): The key inside .mat files that contains the image data.
        
    Returns:
        image_list 

    """
    image_list = []
    
    # Get all .mat files that fit the pattern 
    # Use glob to find all .mat files that fit the pattern 
    # Use "sorted" to make sure gmi/ssmis are read in the same sequence
    mat_files = sorted(glob.glob(file_pattern, recursive=True))
    
    for mat_file in mat_files:
        
        # Load the .mat file
        mat = scipy.io.loadmat(mat_file)
        
        if variable_name not in mat:
            print(f"Warning: {variable_name} not found in {mat_file}, skipping.")
            continue
        
        # Extract the image array
        rate_data = mat[variable_name] # 101x101
        rate=rate_data[:height, :width]  # clip to custom size, e.g., 96x96

        # Don't do this: Keras array_to_img will scale to [0-255] by default
        # Normalize to [0,1] if needed. !! need restore to original scale later  
        # rate_image = rate.astype(np.float32) / np.max(rate) 
        # Don this: 
        rate_image = rate.astype(np.float32) 

        # Convert to a Keras-compatible image
        #image = array_to_img(rate_image)
        #image=rate_image.transpose(Image.FLIP_TOP_BOTTOM)
        
        image=np.flipud(rate_image) 
        image_list.append(image) 
    
        images = np.array(image_list)
    
    return images 

# Example usage:
# train_data, test_data = load_mat_images("path/to/your/mat/files", "your_variable_name")


# returns tf Dataset object 
# if fullset=False, just load a small subset (for testing/debugging, etc.) 

def load_dataset(batch_size=16, fullset=True, height=101, width=101):
   path='/data1/youy/tropics-hurricane/figure/cat1-cat5-tropics-gpm-update-filter-super-reso-v2/'
   if fullset: 
     gmi_data=load_mat_images(path + 'gmi/*.mat', height=height, width=width)
     ssmis_data=load_mat_images(path + 'ssmis/*.mat', height=height, width=width) 
   else: 
     gmi_data=load_mat_images(path + 'gmi/11*.mat', height=height, width=width)
     ssmis_data=load_mat_images(path + 'ssmis/11*.mat', height=height, width=width) 

   # scale to (0, 1) range. save scale for revert back 
   scale=max(np.amax(ssmis_data), np.amax(gmi_data))
   
   gmi=gmi_data/scale 
   ssmis=ssmis_data/scale 

   print("gmi shape: ", gmi.shape, " ssmis shape: ", ssmis.shape)
   print("max scaled gmi value: ", np.amax(gmi), " max scaled ssmis value: ", np.amax(ssmis))

   train_target = gmi[..., tf.newaxis]
   train_data = ssmis[..., tf.newaxis]
 
   print("train_target.shape: ", train_target.shape,  "train_data.shape: ", train_data.shape)
   print("train_target.dtype: ", train_target.dtype,  "train_data.dtype: ", train_data.dtype)  
   print("max train_target: ", str(np.amax(train_target)), "max train_data: ", str(np.amax(train_data)))
   print("min train_target: ", str(np.amin(train_target)), "min train_data: ", str(np.amin(train_data)))

   # Convert to TensorFlow dataset
   batch_size = batch_size 
   dataset = tf.data.Dataset.from_tensor_slices((train_data, train_target)).batch(batch_size) 

   return dataset, scale

