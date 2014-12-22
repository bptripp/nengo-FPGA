
# coding: utf-8

# This is a model for Murphy's FPGA simulator in Nengo.  It is based on the model from Aziz's Master's thesis, but is actually quite different now. This is the description from his thesis, which gives a general idea what's going on.

# 1. Spatial decorrelation from RGCs
# An input signal of continuous intensity values for a moving stimulus is pre-processed
# by convolution with an edge detection filter (see Figure 1.12). This simulates the
# response of the retinal ganglion cells (RGCs) [2].
# 2. Temporal decorrelation from LGN
# Changes in the edge signal vector beyond a threshold (super-threshold events) cause
# a brief (2 ms) burst of spikes representing the moving edge during that interval. This
# simulates the burst output observed in the LGN [9]. See Figure 3.5.
# 3. Directional filtering of V1 input
# The LGN output is projected onto two-dimensional Gabor functions that act as
# spatial direction input filters for the V1 oscillators. This ensures that the initial
# phase angle of x is φ = 0. Each oscillator has an associated direction angle chosen
# from a set of 8, equally spaced between 0 and 2π.
# 41
# Figure 3.1. Conceptual diagram of the proposed ‘oscillator interference’ (OI) model. A description of
# each processing step is included at right. See text for further description.
# 4. Interference with damped, driven phase oscillators in V1
# The V1 simple cell ensembles are driven and damped two-dimensional oscillators
# able to maintain and evolve their state representations over time. They are composed
# of several hundred neurons tuned to the same Gabor direction, but with phase
# tunings distributed from 0 to 2π. Any orthogonal pair of vectors in the phase plane
# are separated by 90 degrees.1 The connectivity of the oscillator neurons is specified
# such that it has a state oscillation frequency ω and damping factor γ.
# The rotation through phase space is restricted to one direction by the dynamics
# matrix detailed in section 3.2.5. Since each oscillator is driven by input and exhibits
# damping properties, its activity decodes to zero in the absence of stimuli.
# 1Any of these pairs constitute the ‘quadrature pair’ proposed by Adelson and Bergen in [19].
# 42
# An initial burst drives the state vector, x(t), to rotate. Subsequent input adds
# vectorially to x(t) and thus interferes with the oscillator state. Constructive interference
# increases the magnitude of x(t) and furthers its rotation. This occurs
# only when the direction and phase of the stimulus is similar to x(t); otherwise, the
# interference is destructive and decreases the magnitude of the state vector. In other
# words, the V1 neurons coding the state vector will exhibit greater activity when
# there is a high correlation between the spatiotemporal frequencies of the stimulus
# and the oscillator state. This is the essential mechanism for spatiotemporal filtering
# in the model and is precisely a resonance interaction between the input bursts and
# oscillator dynamics.2
# 5. Spatial pooling of V1 afferents in MT
# The decoded scalar magnitudes of like-tuned, late-phase (where φ(t) 2 [π, 2π]; see
# Figure 3.1, ‘V1’) oscillator neurons for a number of local patches in the visual
# field are summed in an MT ensemble. This value is the contribution weight of the
# preferred velocity of the MT ensemble to the map of optical flow.

# In[1]:

get_ipython().magic(u'pylab inline')


# In[2]:

import nengo
import numpy as np

model = nengo.Model('Motion')


# In[3]:

#Make the inputs
import cPickle as pickle
import matplotlib.pyplot as plt

#The 'motion_inputs.py' file in this directory will generate this .p file
V1_data = pickle.load(open("LGN-V1.p", "rb"))

def gabor_out(t): 
    dt = .001
    step_num = np.round(t/dt)
    return V1_data[:,step_num]

v1_input = nengo.Node(output=gabor_out, size_in=0, size_out=V1_data.shape[0], label="Gabor")
input_probe = nengo.Probe(v1_input, 'output')

sim = nengo.Simulator(model)
sim.run(.09)
plt.plot(sim.trange(), sim.data(input_probe))
plt.show()


# In[4]:

#Set parameters to define V1 network
width = 1 #number of v1 populations across
height = 1 #number of v1 populations down
n_dirs = 2 #Number of V1 oscillator directions
n_speeds = 3 #Number of V1 oscillator speeds
tcs = np.array([40,100,160])*0.001 #Time constants corresponding to the 20... ms half periods in the thesis for speeds


# In[5]:

#Build the V1 layer given above parameters

#This function builds subnets of n_dir x n_speeds populations that need to be connected to the
#input of aligned LGN elements in a direction to give them the desired preferred direction
def V1_subpopulation():
    #Damped oscillator parameters
    freqs = (2*np.pi)/tcs  #frequencies, Rad/s
    damp = -1.*freqs #corresponding damping values, 2 is critical damping
    V1_tau = 0.02
    V1sub=[]
    V1sub_probe=[]

    for j in range(n_dirs):
        cur_pop = []
        cur_probe = []
        for i in range(n_speeds):
            cur_pop.append(nengo.Ensemble(nengo.LIF(200), 
                                dimensions=2))
            #negate the freqs to make it rotate the other direction (opposite motion)
            A = np.array([[damp[i], freqs[i]], [-freqs[i], damp[i]]])
            #print (freqs[ind]/(2*np.pi)), damp[ind]
            nengo.Connection(cur_pop[i], cur_pop[i], transform=(V1_tau*A+np.eye(2)), filter=V1_tau)  #make it an oscillator
            cur_probe.append(nengo.Probe(cur_pop[i],'decoded_output', filter=V1_tau))
        V1sub_probe.append(cur_probe)
        V1sub.append(cur_pop)
    return V1sub, V1sub_probe

#Make the width x height set of speed/direction tuned populations
V1 = []
V1_probe = []
for i in range(width):
    cur_row=[]
    cur_probe = []
    for j in range(height):
        tmp = V1_subpopulation()
        cur_row.append(tmp[0])
        cur_probe.append(tmp[1])
    V1.append(cur_row)
    V1_probe.append(cur_probe)


# In[6]:

#Connect V1 populations to input. Direction selectivity comes from L/R rotating state vectors
#The gabor outputs are sorted by: x_center, y_center, angles, phases

#Assuming that the input code was run with the same params as above (width, height, etc.)
for i in range(width):
    for j in range(height):
        for m in range(n_dirs):
            #make the right mask for the connections
            mask=np.zeros((2,V1_data.shape[0]))
            ind = i*(height*n_dirs*n_speeds) + j*(n_dirs*n_speeds) + m*n_speeds
            mask[0,ind:ind+n_speeds]=1
            for n in range(n_speeds):
                nengo.Connection(v1_input, V1[i][j][m][n], transform=mask)


# In[7]:

#Build populations of MT neurons that pool over V1
def magnitude(x):
    return x[0]**2 + x[1]**2  #Sqrt?

def connect_MT(width,height,n_wide,n_high):  #n_wide/high is the total pops wide/high
    
    #figure out the right width/height indexes (don't have full 9 at edges and corners)
    w = [width-1, width+1]
    h = [height-1, height+1]
    if width==0:
        w[0]=0
    if width==n_wide-1:
        w[1]=n_wide-1
    if height==0:
        h[0]=0
    if height==n_high-1:
        h[1]=n_high-1

    MT_sub = []
    for i in range(n_dirs):
        cur_pop = []
        for j in range(n_speeds):
            cur_pop.append(nengo.Ensemble(nengo.LIF(100), 
                        dimensions=1))
            #Sum over all 9 V1 oscillators around the center of the MT receptive field, which are aligned in
            #a given direction, for each speed
            scale = (w[1]-w[0]+1)*(h[1]-h[0]+1) #Scale the inputs by the number of pops connected
            for m in range(w[0],w[1]+1):
                for n in range(h[0],h[1]+1):
                    nengo.Connection(V1[m][n][i][j], cur_pop[j], 
                                     function=magnitude, transform=1/scale)  #connect to V1
        MT_sub.append(cur_pop)
    return MT_sub

MT=[]
for i in range(width):
    for j in range(height):
        MT.append(connect_MT(i,j,width,height))


# In[8]:

#Connect the parts of the model
# Create a tester input signal
if 1==0:
    import nengo.helpers

    #This is just temporary to drive some of the V1 oscillators for fun if there's no other input
    input = nengo.Node(output=nengo.helpers.piecewise({0: [10, 0], 0.01: [0, 0]}))

    for j in range(n_dirs):
        for i in range(n_speeds):
            nengo.Connection(input, V1[0][0][j][i])


# In[9]:

#Simulate
sim = nengo.Simulator(model)
sim.run(.099)


# In[10]:

#Plot results
figure(figsize=(4*n_speeds, 4*n_dirs))

for j in range(n_dirs):
    for i in range(n_speeds):
        subplot(n_dirs,n_speeds,(j*n_speeds)+i+1)
        plot(sim.trange(), sim.data(V1_probe[0][0][j][i]))


# In[ ]:



