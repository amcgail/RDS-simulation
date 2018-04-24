# -*- coding: utf-8 -*-
"""
Created on Mon Mar 19 22:51:41 2018

@author: alec
"""

#==============================================================================
# The aim here is to prove these regression methods wrong.
# The idea is that many of the outcome variables are products of network structure.
# Such that the sampling method makes the errors correlated (they were correlated anyways).
#==============================================================================

import simpy
import numpy as np
import random
import networkx as nx

RANDOM_SEED = 42
NUM_PEOPLE = 150
SIM_TIME = 500

class Person(object):
    """
    """
    def __init__(self, env, name):
        self.env = env
        self.name = name
        self.sampled = False
        self.friends = []
        self.sampleInfo = None

    def recruit(self, recruiterInfo={}):
        yield self.env.timeout(random.random()*2+1)
        
        # maybe I never come
        if random.random() > .9:
            return
            
        self.interview(recruiterInfo)
        
        friends = filter( lambda x: not x.sampled, self.friends )
        friends = list(friends)

        coupons = random.randint(0,3)
        
        friends = random.sample(friends, min(coupons,len(friends)))
        for x in friends:
            myinfo = {
                "by": self,
                "when": self.env.now
            }
            self.env.process(x.recruit(myinfo))
    
    def interview(self, recruiterInfo={}):
        self.sampleInfo = recruiterInfo
        self.sampled = True
        return
        
    def __str__(self):
        return self.name
    __repr__ = __str__
        
env = simpy.Environment()

if False:
    people = [Person(env, 'Person %d' % i) for i in range(NUM_PEOPLE)]
    # make friends
    for i, p1 in enumerate(people):
        for j, p2 in enumerate(people):
            if i == j:
                continue
            if random.random() > 0.8:
                p1.friends.append(p2)
                p2.friends.append(p1)
if True:
    PERGROUP = 15
    GROUPS = 5
    people = []
    # make friends
    for i in range(GROUPS):
        newp = [ Person(env, 'Person %d' % i) for i in range(PERGROUP*i, PERGROUP*(i+1)) ]
        for i, p1 in enumerate(newp):
            for j, p2 in enumerate(newp):
                if i == j:
                    continue
                if random.random() > 0.8:
                    p1.friends.append(p2)
                    p2.friends.append(p1)
        people += newp
        
    for i, p1 in enumerate(people):
        for j, p2 in enumerate(people):
            if i == j:
                continue
            if random.random() > 0.99:
                p1.friends.append(p2)
                p2.friends.append(p1)            
     

# start RDS       
seeds = random.sample( people, 3 )
for x in seeds:
    godInfo = {
        "by": "GOD",
        "when": 0
    }
    env.process(x.recruit(godInfo))




# Execute!
env.run(until=SIM_TIME)

# Analyis/results
#==============================================================================
# print('Machine shop results after %s weeks' % WEEKS)
# for p in people:
#     print(p.sampled, p.sampleInfo)
#==============================================================================

G = nx.Graph()
G.add_nodes_from([str(x) for x in people])
G.add_node("GOD")

sampled = filter( lambda x: x.sampled, people )
sampled = list(sampled)
print(len(people), " individuals")
print(len(sampled), " individuals sampled")

G.add_edges_from([(str(x.sampleInfo['by']), str(x)) for x in sampled])

G2 = nx.Graph()
G2.add_nodes_from([str(x) for x in people])
G2.add_edges_from([(str(x), str(y)) for x in sampled for y in x.friends])
G2.add_edges_from([("GOD",str(x)) for x in sampled if x.sampleInfo['by']=='GOD' ])

#==============================================================================
# shells = [["GOD"]]
# while True:
#     
#     thisShell = []
#     for recruiter in shells[-1]:
#         print(recruiter)
#         sampled = [x for x in people if x.sampled]
#         sampled = [x for x in sampled if str(x.sampleInfo['by']) == str(recruiter)]
#         print(sampled)
#         
#         if len(sampled):
#             thisShell += sampled
#     
#     if len(thisShell):
#         shells.append(thisShell)
#     else:
#         break
# 
# shells = [ [str(x) for x in y] for y in shells ]
# 
# pos=nx.shell_layout(G, nlist=shells)
#==============================================================================

pos = nx.spectral_layout(G, weight=10)
#pos = nx.spring_layout(G2, k=0.5, iterations=250)
pos = nx.spring_layout(G, k=2, iterations=500)

#nx.draw(G2,pos,node_size=50,width=0.25)

nx.draw(G,pos, node_size=100,node_color='b')
nx.draw_networkx_nodes(G,pos,nodelist=["GOD"],node_size=300,node_color='r')