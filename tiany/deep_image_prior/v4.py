#! /data1/tiany/miniconda3/bin/python
# v4
# 20250128: do 100K epochs instead of 10K as in v3.
# v3
# 20250126: turn off interactive display of plots, due to x11 issues. 
#           save plots in png files. 
#           use "torch.set_num_threads(1)" to unstick backward()

import numpy			as np
import matplotlib
import matplotlib.pyplot as plt
import cv2			
import torch, torch.nn as nn
from skimage.transform import resize
from skimage.data	import chelsea

torch.set_num_threads(1)  # this solved the hanging backward() problem!
#torch.multiprocessing.set_start_method('spawn') # does not help. still hangs
torch.manual_seed(0);

matplotlib.use('agg') 
plt.rcParams['font.size'] = '14' 
plt.rcParams['toolbar'] = 'None' 
#plt.ion()# will crash x11

device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
print('Device used:', device) 

nxd = 128
chelseaimage = chelsea()
true_object_np = 100.0*resize(chelseaimage[10:299, 110:399, 2], (nxd, nxd), anti_aliasing=False)

print('Image loaded and resized') 
fig1, axs1 = plt.subplots(2, 3, figsize=(20, 12)) # will crash x11 
plt.tight_layout()
#fig1.canvas.manager.window.move(0,0) # optional 

axs1[0, 2].imshow(true_object_np, cmap='Greys_r')
axs1[0, 2].set_axis_off() 

# ==== Now set up a CNN class =============

class CNN(nn.Module):
	def __init__(self, num_channels): 
		super(CNN, self).__init__()
		self.CNN = nn.Sequential(
			nn.Conv2d(1,		num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, num_channels, 3, padding=1), nn.PReLU(), 
			nn.Conv2d(num_channels, 1,              3, padding=1), nn.PReLU() 
		)
	def forward(self, x): return torch.squeeze(self.CNN(x.unsqueeze(0).unsqueeze(0)))

# ==== Now set up a CNN class =============

cnn = CNN(nxd).to(device) # create a CNN object 

print('CNN created') 

input_image = torch.rand(nxd, nxd).to(device)
# ------------ torch to numpy converters 

def torch_to_np(torch_array): return np.squeeze(torch_array.detach().cpu().numpy())
def np_to_torch(np_array):	return torch.from_numpy(np_array).float() 

true_object_torch = np_to_torch(true_object_np).to(device)

# noisy example
measured_data = torch.poisson(true_object_torch)
# add gaps 
mask_image = torch.ones_like(measured_data)
mask_image[int(0.65*nxd):int(0.85*nxd), int(0.65*nxd):int(0.85*nxd)] = 0
mask_image[int(0.15*nxd):int(0.25*nxd), int(0.15*nxd):int(0.35*nxd)] = 0

measured_data = measured_data * mask_image

axs1[0, 2].imshow(torch_to_np(true_object_torch), cmap='Greys_r')
axs1[0, 2].set_title('TRUE') 
axs1[0, 2].set_axis_off() 

axs1[0, 1].imshow(torch_to_np(measured_data), cmap='Greys_r')
axs1[0, 1].set_title('DATA') 
axs1[0, 1].set_axis_off() 

axs1[1, 0].imshow(torch_to_np(input_image), cmap='Greys_r')
axs1[1, 0].set_title('z image %d x %d' % (nxd, nxd)) 
axs1[1, 0].set_axis_off() 


#cv2.waitKey(10000)

def nrmse_fn(recon, reference):
	n = (reference - recon)**2
	den = reference**2
	return 100.0*torch.mean(n)**0.5 / torch.mean(den)**0.5
	#return 100.0*math.sqrt(torch.mean(n)) / math.sqrt(torch.mean(den)) 

# ============== training =========================
optimiser = torch.optim.Adam(cnn.parameters(), lr=1e-4) 
train_loss = list(); 
nrmse_list = list(); 
best_nrmse = 10e9 

print('Training to start') 
for ep in range(100000 +1): 
	optimiser.zero_grad()
	output_image = cnn(input_image) 

	# against the noisy, gappy data
	loss = nrmse_fn(output_image * mask_image, measured_data * mask_image) 

	train_loss.append(loss.item())
	print("ep=%d: loss.backward " % (ep)) 
	loss.backward() # find the gradients 
	print("ep=%d: optimiser.step " % (ep)) 
	optimiser.step() # update parameters
	print("ep=%d: optimiser.step done " % (ep)) 

	nrmse = nrmse_fn(output_image, true_object_torch) # error wrt true image
	nrmse_list.append(nrmse.item()) 

	print("ep=%d " % (ep)) 
	if nrmse < best_nrmse or ep == 0:
		best_recon = output_image;
		best_nrmse = nrmse
		best_ep = ep 
	if ep % 2 == 0:
		print("ep=%d:Recon, NRMSE=%.2f%%" % (ep, nrmse))
		cv2.waitKey(1)
		axs1[1, 2].cla()
		axs1[1, 2].imshow(torch_to_np(best_recon), cmap='Greys_r') 
		axs1[1, 2].set_axis_off() 
		axs1[1, 1].cla() 
		axs1[1, 1].imshow(torch_to_np(output_image), cmap='Greys_r') 
		axs1[1, 1].set_title('Recon %d, NRMSE=%.2f%%' % (ep, nrmse)) 
		axs1[1, 1].set_axis_off() 
		axs1[0, 0].cla() 
		axs1[0, 0].plot(train_loss[-200:-1]) 
		axs1[0, 0].plot(nrmse_list[-200:-1]) 
		axs1[0, 0].set_title('NRMSE (%%), epoch %d' % ep) 
		axs1[0, 0].legend(['Error wrt Data', 'Error wrt TRUE']) 
		print("ep=%d: plot added" % (ep))


plt.savefig('fig_v4.png')
plt.close(fig1)

 

