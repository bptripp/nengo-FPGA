import numpy as np
from scipy import ndimage as nd
import matplotlib.pyplot as plt
import pickle

def make_DOG2(inner_sigma, x):
    y = x
    outer_sigma = inner_sigma*5
    X, Y = np.meshgrid(x, y)
    inner_gaussian = 1./(2.*np.pi*inner_sigma) * np.exp(-(X**2 + Y**2)/2./inner_sigma**2) 
    outer_gaussian = 1./(2.*np.pi*outer_sigma) * np.exp(-(X**2 + Y**2)/2./outer_sigma**2) 
#     return inner_gaussian - outer_gaussian
    return inner_gaussian - outer_gaussian / 2

def make_gabor(x, frequency, phase, sigma): 
    """ Make a Gabor kernel. """
    return np.cos(frequency*x + phase) * np.exp(-x**2/2./sigma**2)

def make_gabors(x):
    """
    Creates 1D Gabor cross-sections 
    
    Returns:
    -------
    gaborx: 3xn cross-sections in sinusoid direction (different phases)
    gabory: 1xn cross-section orthogonal to sinusoid direction    
    """
    gabor_frequency = 2*np.pi/30
    gaborx = np.zeros((len(x), 3))
    gaborx[:,0] = make_gabor(x, gabor_frequency, -np.pi/2, 10)
    gaborx[:,1] = make_gabor(x, gabor_frequency, 0, 10)
    gaborx[:,2] = make_gabor(x, gabor_frequency, np.pi/2, 10)
    
    y = x
    gabory = np.exp(-y**2/2./8**2)
    
    return gaborx, gabory
     
def filter_dots(dots, DOG):
    return nd.filters.convolve(dots, DOG)


def shift_kernel(kernel, shape, centre):
    h, w = kernel.shape
    assert(h % 2 == 1)
    assert(w % 2 == 1)
    half_h = np.floor(h/2)
    half_w = np.floor(w/2)
    
    result = np.zeros((shape[0]+2*half_h, shape[1]+2*half_w)) #zero pad to simplify edge handling 

    ind_h = centre[0] + np.arange(0, 2*half_h+1, dtype='int')    
    ind_w = centre[1] + np.arange(0, 2*half_w+1, dtype='int')
    result[ind_h[:,np.newaxis], ind_w] = kernel
    
    return result[half_h:-half_h,half_w:-half_w]
#     return result[np.ix_(np.arange(half_h))half_h:-half_h][half_w:-half_w]

def mesh(data):
    from mpl_toolkits.mplot3d import Axes3D
    X, Y = np.meshgrid(range(data.shape[1]), range(data.shape[0]))
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    Z = np.squeeze(data)
    ax.plot_surface(X, Y, Z, rstride=2, cstride=2)
    plt.show()
    
def get_DOGs(inner_sigma, x, shape):
    """
    Creates tiled difference-of-gaussian kernels. 
    
    Arguments: 
    inner_sigma: width of narrow +ve gaussian (broad gaussian width is a multiple of this)
    x: 1D domain of gaussian (should be symmetric around 0)
    shape: shape of an image in which we create a DOG centred at each pixel
    
    Returns: 
    Matrix of flattened DOGs. 
    """
    DOG = make_DOG2(inner_sigma, x)
    result = np.zeros((shape[0]*shape[1], x.size**2))
    for i in range(shape[0]): 
        for j in range(shape[1]): 
            k = shift_kernel(DOG, shape, (i,j))
            result[i+shape[0]*j,:] = k.flatten()
    
    return result
    
def find_weights(target, components, rcond=1e-3):
    gamma = np.dot(components, components.T)
    inv_gamma = np.linalg.pinv(gamma, rcond) #I think this has to scale with size 
    return np.dot(inv_gamma, np.dot(components, target))

def get_inputs(w_kernel, filt_stim, centres):
    """
    Arguments: 
    w_kernel: DOG->Gabor weight kernel
    filt_stim: 
    centres: list of (vertical, horizontal) coords for V1 RF centres
    
    Returns: 
    Weighted net inputs for V1 receptive fields (# RFs x # Gabors x #time steps)
    """
    nrf = centres.shape[0] # number of receptive fields
    ng = w_kernel.shape[2] # number of gabors per receptive field
    nf = filt_stim.shape[2] # number of frames
    
    # offsets (from RF centres) of subimages to multiply with kernels
    vw = int(np.floor(w_kernel.shape[0]/2))
    v_offsets = np.arange(-vw, vw+1)
    hw = int(np.floor(w_kernel.shape[1]/2))
    h_offsets = np.arange(-hw, hw+1)
    
    threshold = 20
    threshold_crossings = np.zeros_like(filt_stim)
    for i in range(1, nf): 
        increasing = filt_stim[:,:,i] > filt_stim[:,:,i-1]
        above_threshold = filt_stim[:,:,i] > threshold
        threshold_crossings[:,:,i] = np.logical_and(increasing, above_threshold)

    
    sample_hold_ratio = 10
    burst_len = 6 # duration of LGN bursts (time steps)
    result = np.zeros((nrf, ng, nf*sample_hold_ratio))
    for i in range(nf): 
        for j in range(nrf): 
            v_indices = v_offsets + centres[j,0]
            h_indices = h_offsets + centres[j,1]
            region = threshold_crossings[v_indices[:,np.newaxis],h_indices,i]
            for k in range(ng): 
                net_input = np.sum(w_kernel[:,:,k].T * region)
                for l in range(burst_len): 
                    result[j, k, i*sample_hold_ratio+l] = net_input 
                
    result = np.maximum(0, result)
    return result

def get_default_weights():
    # find & save optimal approximation weights ... 
    x = np.arange(-40, 41, 1)
    DOGs = get_DOGs(3, x, (x.size, x.size))
    gaborx, gabory = make_gabors(x)
    n_gabors = gaborx.shape[1]
    w_kernel = np.zeros((gaborx.shape[0], gabory.size, n_gabors))
    for i in range(n_gabors):
        gabor = np.outer(gabory, gaborx[:,i])
        w = find_weights(gabor.flatten(), DOGs, rcond=1e-2)
        w_kernel[:,:,i] = np.reshape(w, (x.size, x.size))
    pickle.dump(w_kernel, open("/Users/bptripp/code/nengo-FPGA/v1/dog-gabor-weights-new-2.p", "wb" ))
    return w_kernel

def check_gabor_approx():
    # plot approximations ... 
    x = np.arange(-40, 41, 1)
    DOG2 = make_DOG2(3, x)
    DOGs = get_DOGs(3, x, (x.size, x.size))
    gaborx, gabory = make_gabors(x)
    n_gabors = gaborx.shape[1]

    w_kernel = pickle.load(open("/Users/bptripp/code/nengo-FPGA/v1/dog-gabor-weights-new.p", "rb" ))

    plt.figure()
    for i in range(n_gabors):
        gabor = np.outer(gabory, gaborx[:,i])
        approx = np.dot(DOGs, w_kernel[:,:,i].flatten())
        plt.subplot(3,n_gabors,i+1)
        plt.imshow(gabor)
        plt.subplot(3,n_gabors,i+n_gabors+1)
        plt.imshow(np.reshape(approx, (x.size, x.size)))
        plt.subplot(3,n_gabors,i+2*n_gabors+1)    
        plt.imshow(nd.filters.convolve(w_kernel[:,:,i].T, DOG2))
    plt.show()

 
def filter_default_dots():
    # filter dot stimulus with DOG filter ...
    x = np.arange(-40, 41, 1)
    DOG2 = make_DOG2(3, x)  
    dots = pickle.load(open("/Users/bptripp/code/nengo-FPGA/v1/dot-images-coh1-2000ms-s02.p", "rb" ), encoding='latin1')  
    dots = dots - np.min(dots)
    filtered = np.zeros_like(dots)
    for i in range(dots.shape[2]):
        print('processing frame ' + str(i))    
        filtered[:,:,i] = filter_dots(np.squeeze(dots[:,:,i]), DOG2)
     
    pickle.dump(filtered, open("/Users/bptripp/code/nengo-FPGA/v1/dog-filtered-dots-new.p", "wb" ))
    
def pure_gabor():
    dots = pickle.load(open("/Users/bptripp/code/nengo-FPGA/v1/dot-images-coh1-2000ms-s02.p", "rb" ), encoding='latin1')  
    x = np.arange(-40, 41, 1)
    gaborx, gabory = make_gabors(x)
    centres = np.array([[200,200]])
    
    nf = dots.shape[2]
    nrf = centres.shape[0]  # number of receptive fields
    ng = gaborx.shape[1] # number of gabors per receptive field
    
    # offsets (from RF centres) of subimages to multiply with kernels
    vw = int(np.floor(gabory.size/2))
    v_offsets = np.arange(-vw, vw+1)
    hw = int(np.floor(gaborx.shape[0]/2))
    h_offsets = np.arange(-hw, hw+1)
 
    result = np.zeros((nrf, ng, nf))
    for i in range(dots.shape[2]): 
        for j in range(nrf): 
            v_indices = v_offsets + centres[j,0]
            h_indices = h_offsets + centres[j,1]
            region = dots[v_indices[:,np.newaxis],h_indices,i]
            for k in range(ng): 
                gabor = np.outer(gabory, gaborx[:,k])
                result[j,k,i] = np.sum(gabor * region)
    return result

def get_default_inputs():
#     w_kernel = pickle.load(open("/Users/bptripp/code/nengo-FPGA/v1/dog-gabor-weights-3.p", "rb" ))
#     w_kernel = pickle.load(open("/Users/bptripp/code/nengo-FPGA/v1/dog-gabor-weights-2.p", "rb" ))
#     w_kernel = pickle.load(open("/Users/bptripp/code/nengo-FPGA/v1/dog-gabor-weights-4.p", "rb" ))
#     filtered = pickle.load(open("/Users/bptripp/code/nengo-FPGA/v1/dog-filtered-dots.p", "rb" ))
    w_kernel = pickle.load(open("/Users/bptripp/code/nengo-FPGA/v1/dog-gabor-weights-new-2.p", "rb" ))
    filtered = pickle.load(open("/Users/bptripp/code/nengo-FPGA/v1/dog-filtered-dots-new.p", "rb" ))
    
    c = np.arange(87, 313, 25)
    centres = np.zeros((c.size**2,2), dtype='int')
    for i in range(c.size): 
        for j in range(c.size): 
            centres[i*c.size+j,0] = c[i]
            centres[i*c.size+j,1] = c[j]
    
    print(centres)
    
#     centres = np.array([[200,200]]) 
    return get_inputs(w_kernel, filtered, centres)


# get_default_weights()

inputs = get_default_inputs()
pickle.dump(inputs, open("/Users/bptripp/code/nengo-FPGA/v1/inputs.p", "wb" ))
# inputs = pure_gabor()
plt.plot(inputs[0,0,:]-10, 'r')
plt.plot(inputs[0,1,:], 'g')
plt.plot(inputs[0,2,:]+10, 'b')
plt.show()

