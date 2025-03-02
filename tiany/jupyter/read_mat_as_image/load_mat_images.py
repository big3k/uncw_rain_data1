
import os
import scipy.io
import numpy as np
from PIL import Image
from tensorflow.keras.preprocessing.image import array_to_img
from sklearn.model_selection import train_test_split

def load_mat_images(directory, variable_name='moved'):
    """
    Load all .mat files in a directory as images and split into training and test sets.
    
    Parameters:
        directory (str): Path to the directory containing .mat files.
        variable_name (str): The key inside .mat files that contains the image data.
        
    Returns:
        train_images (numpy.ndarray): 70% of images for training.
        test_images (numpy.ndarray): 30% of images for testing.
    """
    image_list = []
    
    # Get all .mat files in the directory
    mat_files = [f for f in os.listdir(directory) if f.endswith(".mat")]
    
    for mat_file in mat_files:
        mat_path = os.path.join(directory, mat_file)
        
        # Load the .mat file
        mat = scipy.io.loadmat(mat_path)
        
        if variable_name not in mat:
            print(f"Warning: {variable_name} not found in {mat_file}, skipping.")
            continue
        
        # Extract the image array
        rate_struct = mat[variable_name]
        #print(type(rate_struct))
        fields = rate_struct.dtype.names
        rate_data = rate_struct[0, 0]['rate']  # get rain rate 
        # clip to the center for now, 100x100
        rate=rate_data[100:200, 100:200]

        # Check for NaN values and skip file if found
        if np.isnan(rate).any():
            print(f"Warning: NaN values found in {mat_file}, skipping.")
            continue

        # Normalize to [0,1] if needed. !! need restore to original scale later  
        rate_image = rate.astype(np.float32) / np.max(rate) 

        # Reshape if necessary (e.g., for grayscale images)
        if len(rate_image.shape) == 3:
            # Assume (height, width, channels)
            rate_image = np.expand_dims(rate_image, axis=0)  # Add batch dimension if needed
        elif len(rate_image.shape) == 2:
            # Grayscale image (height, width) -> (height, width, 1)
            rate_image = np.expand_dims(rate_image, axis=-1)

        # Convert to a Keras-compatible image
        image = array_to_img(rate_image)
        image=image.transpose(Image.FLIP_TOP_BOTTOM)
        
        image_list.append(image) 
    
    # Convert list to NumPy array
    images = np.array(image_list)
    
    # Split into 70% training and 30% testing
    train_images, test_images = train_test_split(images, test_size=0.3, random_state=42)
    
    return train_images, test_images

# Example usage:
# train_data, test_data = load_mat_images("path/to/your/mat/files", "your_variable_name")

