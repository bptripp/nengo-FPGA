# -*- coding: utf-8 -*-
import cPickle as pickle
import numpy as np
import nengo
import matplotlib.pyplot as plt

V1_data = pickle.load( open("LGN-V1.p", "rb" ) )

def gabor_out(t): 
    dt = .001
    step_num = np.round(t/dt)
    return V1_data[:,step_num]

model = nengo.Model(label="motion")        

v1_input = nengo.Node(output=gabor_out, size_in=0, size_out=V1_data.shape[0], label="Gabor")
input_probe = nengo.Probe(v1_input, 'output')

sim = nengo.Simulator(model)
sim.run(.09)
plt.plot(sim.trange(), sim.data(input_probe))
plt.show()
