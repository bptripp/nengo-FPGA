from psychopy import visual
import numpy as np
from scipy import ndimage as nd
import cPickle as pickle

# Note: I've been running this in Python 2.7 -- not sure if PsychoPy works in Python 3

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
        nDots=15, dotSize=5,
        speed=speed, dir=direction, coherence=coherence,
        fieldPos=[0.0, 0.0], fieldSize=10,fieldShape='circle',
        signalDots='same', noiseDots='position',dotLife=10,
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

#  make dot stimulus and associated V1 inputs ... 
np.random.seed(seed=1) 
n_frames = 201
dot_images = make_dots(n_frames, coherence=1, speed=0.04, direction=180)
pickle.dump(dot_images, open("dot-images-coh1-s04.p", "wb"))

