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
import matplotlib.pyplot as plt

RANDOM_SEED = 42
NUM_PEOPLE = 5000
SIM_TIME = 500

MAX_SAMPLE = 100

num_sampled = 0

class Person(object):
    """
    """
    def __init__(self, env, index):
        self.env = env
        self.index = index
        self.sampled = False
        self.friends = []
        self.sampleInfo = None

    def recruit(self, recruiterInfo={}):
        yield self.env.timeout(random.random()*2+1)
        
        # maybe I never come
        if random.random() > .8:
            return
            
        self.interview(recruiterInfo)
    
    def interview(self, recruiterInfo={}):
        global num_sampled

        if num_sampled >= MAX_SAMPLE:
            return

        self.sampleInfo = recruiterInfo
        self.sampled = True
        
        num_sampled += 1
        
        friends = filter( lambda x: not x.sampled, self.friends )
        friends = list(friends)

        # how many I decide to give out...
        coupons = random.randint(0,3)
        
        friends = random.sample(friends, min(coupons,len(friends)))
        for x in friends:
            myinfo = {
                "by": self,
                "when": self.env.now
            }
            self.env.process(x.recruit(myinfo))
        
    def __str__(self):
        return 'Person %d' % self.index
    __repr__ = __str__
    
def simpleScatter(x,y):
    plt.scatter( x, y )
    plt.axis([np.min(x), np.max(x), np.min(y), np.max(y)])    
    
def drawGraph(people, drawFull=False, springSamp=True, springPop=False):
    sampled = filter( lambda x: x.sampled, people )
    sampled = list(sampled)
    
    G = nx.Graph()
    G.add_nodes_from([str(x) for x in sampled])
    G.add_node("GOD")
    
    G.add_edges_from([(str(x.sampleInfo['by']), str(x)) for x in sampled])
    
    #pos = nx.spectral_layout(G, weight=10)
    #pos = nx.spring_layout(G2, k=0.5, iterations=250)
    if springSamp:
        pos = nx.spring_layout(G, k=2, iterations=500)
    else:
        pos = nx.random_layout(G)
    
    if drawFull:
        G2 = nx.Graph()
        G2.add_nodes_from([str(x) for x in people])
        G2.add_edges_from([(str(x), str(y)) for x in sampled for y in x.friends])
        G2.add_edges_from([("GOD",str(x)) for x in sampled if x.sampleInfo['by']=='GOD' ])
        
        if springPop:
            pos = nx.spring_layout(G2, pos=pos, k=2, iterations=250)
        else:
            pos = nx.random_layout(G, pos=pos)
        nx.draw(G2,pos,node_size=50,width=0.25)
    
    nx.draw(G,pos, node_size=100, node_color='b', width=2)
    return nx.draw_networkx_nodes(G,pos,nodelist=["GOD"],node_size=300,node_color='r')    
    
if "ests" not in locals() or True:
    ests = []

while len(ests) < 50:
    print(len(ests), "th iteration")
    num_sampled = 0
    
    env = simpy.Environment()
    
    # now model some outcome for each individual.
    nppl = NUM_PEOPLE
    black = np.random.binomial(1, 0.5, nppl)
    educ = np.random.normal(12, 3, nppl)
    ability = np.random.normal(8, 2, nppl)
    
    cov = np.matrix([
        black,
        educ,
        ability
    ])
    
    lnwage = 9.49 + 0.046*educ - 0.165*black + 0.023*np.multiply(educ,black) + \
        0.024*ability + np.random.normal(0, 0.25, nppl)
    wage = np.exp(lnwage)    
    
    if False:
        # random
    
        people = [Person(env, i) for i in range(NUM_PEOPLE)]
        # make friends
        for i, p1 in enumerate(people):
            for j, p2 in enumerate(people):
                if i == j:
                    continue
                if random.random() > 0.8:
                    p1.friends.append(p2)
                    p2.friends.append(p1)
    if False:
        # groups
    
        GROUPS = 5
        PERGROUP = int(NUM_PEOPLE / GROUPS)
        people = []
        # make friends
        for i in range(GROUPS):
            newp = [ Person(env, i) for i in range(PERGROUP*i, PERGROUP*(i+1)) ]
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
                    
    if True:
        # based on covariates

        people = [Person(env, i) for i in range(NUM_PEOPLE)]
        
        NFRIENDSHIPS = NUM_PEOPLE * (NUM_PEOPLE - 1) / 2
        NFRIENDSHIPS *= 0.02
        NFRIENDSHIPS = int(NFRIENDSHIPS)
        
        nf = 0
        # make friends
        while nf < NFRIENDSHIPS:
            p1 = np.random.choice(people)
            myCov = cov[:,p1.index]
            
            homophily = 1
            
            logodds = 0
            logodds += homophily * ( cov[0,:] == myCov[0] )*0.5             # same race
            logodds += - homophily * np.abs( cov[1,:] - myCov[1] ) / ( np.max(cov[1,:]) - np.min(cov[1,:]) )   # distance in educ
            logodds += - homophily * np.abs( cov[2,:] - myCov[2] ) / ( np.max(cov[2,:]) - np.min(cov[2,:]) )   # distance in ability
            logodds += - homophily * np.abs( wage - wage[p1.index] ) / ( np.max(wage) - np.min(wage) )   # distance in dep var
            
            odds = np.exp(logodds)
            p = odds / (1 + odds)
            p = np.multiply( p, 1./np.sum(p) )
            p = np.array(p)[0,:]
            
            p2 = np.random.choice(people, p=p)
            
            if p1 not in p2.friends:
                p1.friends.append(p2)
                p2.friends.append(p1)
                nf += 1

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
    
    sampled = filter( lambda x: x.sampled, people )
    sampled = list(sampled)
    
    if True:
        #print(len(people), " individuals")
        print(len(sampled), " individuals sampled")
    
    if len(sampled) < 50:
        continue
    
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
    
    
    if False:
        print("At spring layout")
        print("")
        
        drawGraph(people)
    
    samplei = [x.index for x in sampled]
    cov = np.matrix([
        educ[ samplei ],
        black[ samplei ],
        np.multiply(educ,black)[ samplei ],
        ability[ samplei ],
        np.zeros( len(samplei) ) + 1
    ]).transpose()
    dep = lnwage[ samplei ]
    
    res = np.linalg.lstsq( cov, dep )
    ests.append(res[0])
    
estM = np.matrix(ests)

if False:
    plt.hist(estM[:,5])
    
for i in range(estM.shape[1]):
    print( np.mean( estM[:,i] ) )