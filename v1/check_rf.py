# Finds empirical receptive field of first Gabor. This assumes
# we have weights, threshold, DOG, etc. from running motion_inputs.py

import numpy as np
from scipy import ndimage as nd
import matplotlib.pyplot as plt

#probe dot centres ... 
dot_ind = range(-196, 197, 2) #don't probe edges
dotx = np.tile(dot_ind, (len(dot_ind), 1))
doty = dotx.transpose()
dotx_list = np.ndarray.flatten(dotx)
doty_list = np.ndarray.flatten(doty)

img_ind = range(-200, 200, 1)
imgx = np.tile(img_ind, (len(img_ind), 1))
imgy = imgx.transpose()

dd_prev = np.zeros_like(imgx)
LGN = np.zeros_like(imgx)
threshold = 0.5
V1_RF = np.zeros_like(dotx)
DOG = make_DOG(dog_inner_sigma, np.arange(-75,75)) 

for i in range(len(dotx_list)):
    print i

    img = np.zeros_like(imgx)
    radius = 2
    mask = (imgx-dotx_list[i])**2+(imgy-doty_list[i])**2 <= radius**2
    img[mask] = 255

    dd1 = nd.filters.convolve1d(img, DOG, axis=0)    
    dd2 = nd.filters.convolve1d(dd1, DOG, axis=1)

    LGN[dd2 > threshold] = 1    
    LGN_out = LGN.take(dog_indices, axis=0).take(dog_indices, axis=1)
    V1 = np.dot(np.reshape(LGN_out, (1,-1)), weights)
    V1_RF[dotx_list[i], doty_list[i]] = V1[0][0]
    
