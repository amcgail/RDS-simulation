# -*- coding: utf-8 -*-
"""
Created on Thu Apr 19 14:19:53 2018

@author: alec
"""

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
NUM_PEOPLE = 500
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
            
    def addFriend(self, p):
        if p in self.friends:
            return
        
        self.friends.append(p)
        p.friends.append(self)
        
    def __str__(self):
        return 'Person %d' % self.index
    __repr__ = __str__
    
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
    
people = [Person(env, i) for i in range(NUM_PEOPLE)]

# add dealer relationships
dealers = random.sample(people, 10)
for p in people:
    if p in dealers:
        continue
    
    myd = random.choice(dealers)
    p.addFriend( myd )
    
# another idea is to have IDU status transmit along social networks (as is probable)

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
        p1.addFriend(p2)
        nf += 1