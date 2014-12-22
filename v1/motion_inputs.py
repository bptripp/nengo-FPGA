from psychopy import visual
import numpy as np
from scipy import ndimage as nd
import cPickle as pickle

def make_DOG(inner_sigma, x): 
    """ Make a 1D difference-of-Gaussians kernel. """
    outer_sigma = inner_sigma*4
    inner_gaussian = 1/np.sqrt(2*np.pi)/inner_sigma * np.exp(-x**2/2/inner_sigma**2)
    outer_gaussian = 1/np.sqrt(2*np.pi)/outer_sigma * np.exp(-x**2/2/outer_sigma**2)
    return inner_gaussian - outer_gaussian

def make_DOG2D(inner_sigma, x, y, centre):
    """ Make a 2D difference-of-Gaussians kernel. """
    outer_sigma = inner_sigma*4
    inner_gaussian = 1/(2*np.pi*inner_sigma**2) * np.exp(-((x-centre[0])**2 + (y-centre[1])**2)/2/inner_sigma**2)
    outer_gaussian = 1/(2*np.pi*outer_sigma**2) * np.exp(-((x-centre[0])**2 + (y-centre[1])**2)/2/outer_sigma**2)
    return inner_gaussian - outer_gaussian

def make_gabor(x, y, centre, frequency, phase, sigma, angle): 
    """ Make a Gabor kernel. """
    xx = x*np.cos(angle) - y*np.sin(angle)
    yy = x*np.sin(angle) + y*np.cos(angle)
    xx = xx-centre[0]
    yy = yy-centre[1]
    return np.sin(frequency*xx + phase) * np.exp(-xx**2/2/sigma[0]**2 -yy**2/2/sigma[1]**2)

#Assuming frame rate of 100Hz (will sample-and-hold for 1000Hz neural simulation)  
def make_dots(n_frames, coherence=0.5, speed=0.1, direction=180):
    """
    Make a random dot stimulus. 
    
    Args: 
      n_frames (int): number of frames in stimulus
    
    Keyword args:
      coherence (float): dot motion coherence (between 0 and 1)
      speed (float): dot speed in strange units
      direction (float): coherent dot motion angle in degrees
    
    Returns: 
      ndarray: dot image frames (400 x 400 x n_frames)
    """    
    im_size = (400,400);
    
    win = visual.Window(size=im_size, fullscr=False, screen=0, allowGUI=False, allowStencil=False,
        monitor='testMonitor', color=[0,0,0], colorSpace='rgb',
        blendMode='avg', useFBO=True,
        )

    dots = visual.DotStim(win=win, name='dots',units='deg', 
        nDots=200, dotSize=5,
        speed=speed, dir=direction, coherence=coherence,
        fieldPos=[0.0, 0.0], fieldSize=10,fieldShape='circle',
        signalDots='same', noiseDots='direction',dotLife=3,
        color=[1.0,1.0,1.0], colorSpace='rgb', opacity=1, depth=-1.0)

    for frameN in range(n_frames):
        dots.frameNStart = frameN
        win.flip()
        dots.draw(win)
        win.getMovieFrame(buffer='back')

    win.close()

    dot_images = np.zeros((im_size[0], im_size[1], n_frames))
    for frameN in range(n_frames):
        frame_data = np.array(list(win.movieFrames[frameN].getdata()))
        dot_images[:,:,frameN] = frame_data[:,0].reshape(im_size)
        
    return dot_images

def make_DOGs(inner_sigma, centres, x, y):
    """ Make a grid of 2D difference-of-Gaussian kernels, flattened into vectors. """
    DOGs = np.zeros((x.shape[0]*x.shape[1], len(centres)**2))
    c = 0
    for cx in centres: 
        for cy in centres: 
            DOG = make_DOG2D(inner_sigma, x, y, (cx,cy))
            DOGs[:,c] = np.reshape(DOG, -1)
            c = c + 1            
    return DOGs
    
def make_gabors(centres, angles, phases, frequency, sigma, x, y):
    """ 
    Make Gabor kernels with all combinations of angles, phases, etc., flattened into vectors.
    
    Args: 
      centres: list of centre pixels for kernels, along one dimension (0 at image centre)
      angles: list of rotation angles in radians
      phases: list of Gabor phases (radians)
      frequency: frequency of sinusoidal component (cycles/pixel)
      sigma: 2-element list with gaussian width (parallel,orthogonal) to sinusoidal component
      x: grid of horizontal offsets (pixels)
      y: grid of horizontal offsets (pixels)
    
    Returns: 
      ndarray: matrix of flattened 2D gabor kernels with all combinations of centres, 
        angles, and phases. See code re. ordering of different kernels.                 
    """
    gabors = np.zeros((x.shape[0]*x.shape[1], len(centres)**2*len(angles)*len(phases)))
    c = 0;
    for cx in centres:
        for cy in centres:
            for angle in angles:
                for phase in phases:
                    gabor = make_gabor(x, y, (cx,cy), frequency, phase, sigma, angle)
                    gabors[:,c] = np.reshape(gabor, (-1))
                    c = c + 1
    return gabors
    
def dog_to_gabor_weights(DOGs, gabors):
    """ Find weights for combining DOG receptive fields into a set of Gabors in LGN-V1 projection."""
    gamma = np.dot(DOGs.T, DOGs)
    inv_gamma = np.linalg.pinv(gamma)
    weights = np.dot(inv_gamma, np.dot(DOGs.T, gabors))
    return weights

# Domain of RFs for finding LGN-V1 weights ... 
ind = range(-200, 201, 2) #image vectors are pretty big without subsampling
x = np.tile(ind, (len(ind), 1))
y = x.transpose()

# RF parameters ... 
gabor_centres = np.linspace(-150, 150, 4)
gabor_angles = np.linspace(0, np.pi, 4)
gabor_phases = np.linspace(0, np.pi, 4)
dog_centres = range(-195, 200, 5) #make matrix reasonable size by using DOGs centred every few pixels
dog_inner_sigma = 6 #for difference-of-gaussians
gabor_frequency = 2*np.pi/32
gabor_sigma = (10,20)

# find weights to approximate Gabor RFs from difference-of-gaussian RFs ... 
DOGs = make_DOGs(dog_inner_sigma, dog_centres, x, y)
gabors = make_gabors(gabor_centres, gabor_angles, gabor_phases, gabor_frequency, gabor_sigma, x, y)
weights = dog_to_gabor_weights(DOGs, gabors);

# make dot stimulus and associated V1 inputs ... 
n_frames = 10
dot_images = make_dots(n_frames, coherence=0.5, speed=0.1, direction=180)
sample_hold_ratio = 10 #number of neural simulation samples per stimulus frame
im_size = dot_images.shape[0:2]
dog_indices = (np.array(dog_centres) + 200).tolist()
DOG = make_DOG(dog_inner_sigma, np.arange(-75,75)) #1D for faster image convolution (the 2D kernel is separable)
LGN = np.zeros((im_size[0], im_size[1], n_frames*sample_hold_ratio))
V1 = np.zeros((gabors.shape[1], n_frames*sample_hold_ratio))
threshold = 0.5

dd_prev = np.zeros(im_size)
for frame_num in range(n_frames): 
    dd1 = nd.filters.convolve1d(dot_images[:,:,frame_num], DOG, axis=0)    
    dd2 = nd.filters.convolve1d(dd1, DOG, axis=1)

    for step_num in range(sample_hold_ratio): 
        threshold_crossings = np.logical_and(dd2 > dd_prev, dd2 > threshold)
        ind = frame_num*sample_hold_ratio+step_num

        LGN[threshold_crossings,ind] = 1
        if ind < n_frames*sample_hold_ratio-1: #Aziz uses a 2-ms burst at each threshold crossing
            LGN[threshold_crossings,ind+1] = 1
        
        LGN_ind = LGN[:,:,ind]
        LGN_out = LGN_ind.take(dog_indices, axis=0).take(dog_indices, axis=1)
        V1[:,ind] = np.dot(np.reshape(LGN_out, (1,-1)), weights)
            
        dd_prev = dd2
        
pickle.dump(V1, open("LGN-V1.p", "wb"))
