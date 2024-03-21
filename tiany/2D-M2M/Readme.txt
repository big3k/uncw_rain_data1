1/21/2024: 

N19_PSF: Try to do deconvolution on M2M, just to see what it produces. 

Ideally deconv shall be applied first to the sounder, then train the M2M model. 
But since the M2M model is already trained from the 1-D sounder data, 
and currently there are not enough 2D sounder cases to retrain the model, 
we can not do deconv sounder first then M2M. 

1/15/2024

N19:  Test M2M algorithm for 2D rain fields, with model based on Exp. 6d of 
   https://lswg.uncw.edu/group/M2M/phase2/




