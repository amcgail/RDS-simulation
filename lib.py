#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
THIS IS NOT QUITE FINISHED

OK... what exactly do I want to happen
I have a bunch of individuals, they have friends, etc.
This is a "network"
Then I create an RDS sample from this data,
    recording who recruited whom, when, etc.
"""

import simpy
import random
import numpy as np
from csv import writer
from itertools import chain
import networkx as nx

class Person(object):

    def __init__(self, index):
        self._prop = {}
        self.friends = []
        self.index = index
        
        self.sampled = False
        self.sampleInfo = None

    def addFriend(self, p):
        if p not in self.friends:
            self.friends.append(p)
            p.friends.append( self )


    def recruit(self, env, recruiterInfo={}):
        yield env.timeout(random.random()*2+1)

        # maybe I never come
        if random.random() > .8:
            return

        self.interview(env, recruiterInfo)

    def interview(self, env, recruiterInfo={}):
        if env.num_sampled >= env.maxsample:
            return

        self.sampleInfo = recruiterInfo
        self.sampled = True

        env.num_sampled += 1
        #print( self.index, "sampled :)" )

        friends = filter( lambda x: not x.sampled, self.friends )
        friends = list(friends)

        # how many I decide to give out...
        coupons = random.randint(0,3)

        friends = random.sample(friends, min(coupons,len(friends)))
        for x in friends:
            myinfo = {
                "by": self,
                "when": env.now
            }
            env.process(x.recruit(env, myinfo))

    def props(self):
        return [self._prop[k] for k in self._prop if type(self._prop[k]) in [str, int, bool]]
    def propkeys(self):
        return [k for k in self._prop if type(self._prop[k]) in [str, int, bool]]

    def __str__(self):
        return 'Person "%d"' % self.index
    __repr__ = __str__

    def __getitem__(self, x):
        return self._prop[x]
    def __setitem__(self, x, y):
        self._prop[x] = y
    def __delitem__(self, x):
        del self._prop[x]
    def __contains__(self, x):
        return x in self._prop
    def __len__(self):
        return len(self._prop)

class Network():
    @classmethod
    def fromEdgeList(cls, edgeList):
        try:
            edgeList = list(edgeList)
            nodes = set( chain.from_iterable( edgeList ) )
        except:
            raise Exception("It seems you didn't give me an edgeList...")

        net = cls()
        for name in nodes:
            net.addPerson( Person(name) )

        for edge in edgeList:
            net.pdict[edge[0]].addFriend( net.pdict[edge[1]] )

        return net

    def addPerson( self, p ):
        self.people.append( p )
        self.pdict[ p.index ] = p

    def __init__(self):
        self.people = []
        self.pdict = {}
        
    def addPeople( self, n ):
        index = 0
        for i in range(n):
            while index in self.pdict:
                index += 1
                
            self.addPerson( Person( index ) )

    def genOLSwage(self):
        # generate covariates which we use as starters
        nppl = len( self.people )

        black = np.random.binomial(1, 0.5, nppl)
        educ = np.random.normal(12, 3, nppl)
        ability = np.random.normal(8, 2, nppl)

        lnwage = 9.49 + 0.046*educ - 0.165*black + 0.023*np.multiply(educ,black) + \
            0.024*ability + np.random.normal(0, 0.25, nppl)
        self.wage = np.exp(lnwage)

        for i, p in enumerate(self.people):
            p['black'] = black[i]
            p['educ'] = educ[i]
            p['ability'] = ability[i]
            p['wage'] = self.wage[i]

    def makeFriends(self):
        # this hasn't really been transferred

        NUM_PEOPLE = len(self.people)
        NFRIENDSHIPS = NUM_PEOPLE * (NUM_PEOPLE - 1) / 2
        NFRIENDSHIPS *= 0.02
        NFRIENDSHIPS = int(NFRIENDSHIPS)

        attr = self.people[0]._prop.keys()
        cov = np.matrix([ [ p._prop[a] for p in self.people ] for a in attr ])

        homophily = 1

        nf = 0
        # make friends
        while nf < NFRIENDSHIPS:
            p1 = random.choice(self.people)
            
            myCov = cov[:,p1.index]

            logodds = 0
            
            for i in range(len(attr)):
                logodds += - homophily * ( cov[i,:] - myCov[i] ) / ( np.max(cov[1,:]) - np.min(cov[1,:]) )   # distance decreases logodds

            odds = np.exp(logodds)
            p = odds / (1 + odds)
            p = np.multiply( p, 1./np.sum(p) )
            p = np.array(p)[0,:]

            p2 = np.random.choice(self.people, p=p)

            if p1 not in p2.friends:
                p1.friends.append(p2)
                p2.friends.append(p1)
                nf += 1

    def clearRDS(self):
        for p in self.people:
            p.sampled = False
            p.sampleInfo = None

    def performRDS(self, numseeds=3, maxsample=100):

        SIM_TIME = 500

        self.clearRDS()

        env = simpy.Environment()
        env.num_sampled = 0
        env.maxsample = maxsample

        # start RDS
        seeds = random.sample( self.people, numseeds )
        for x in seeds:
            godInfo = {
                "by": "GOD",
                "when": 0
            }
            env.process(x.recruit(env, godInfo))

        # Execute!
        env.run(until=SIM_TIME)

        # Analyis/results
        sampled = filter( lambda x: x.sampled, self.people )
        sampled = list(sampled)

        print(len(sampled), " individuals sampled")

        if len(sampled) < maxsample / 2:
            print( "..sample too small. restarting")
            return self.performRDS(numseeds=numseeds)

    def RDS_CSV(self, fn):
        with open(fn, 'w') as outf:
            outc = writer(outf)
            outc.writerow(['recruiter', 'recruit', 'degree'] + self.people[0]._prop.keys())
            for p in self.people:
                if not p.sampled:
                    continue

                r = None
                if p.sampleInfo['by'] != "GOD":
                    r = p.sampleInfo['by'].index

                outc.writerow([ r, p.index, len(p.friends) ] + p._prop.values())
    
    def full_CSV(self, fn):
        with open(fn, 'w') as outf:
            outc = writer(outf)
            print(self.people[0]._prop.keys())
            outc.writerow(['id', 'degree'] + self.people[0]._prop.keys())
            for p in self.people:
                outc.writerow([p.index, len(p.friends)] + p._prop.values())

    def plotRecruitment(self, drawFull=False, springSamp=True, springPop=False):
        sampled = filter( lambda x: x.sampled, self.people )
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
            G2.add_nodes_from([str(x) for x in self.people])
            G2.add_edges_from([(str(x), str(y)) for x in sampled for y in x.friends])
            G2.add_edges_from([("GOD",str(x)) for x in sampled if x.sampleInfo['by']=='GOD' ])

            if springPop:
                pos = nx.spring_layout(G2, pos=pos, k=2, iterations=250)
            else:
                pos = nx.random_layout(G, pos=pos)
            nx.draw(G2,pos,node_size=50,width=0.25)

        nx.draw(G,pos, node_size=100, node_color='b', width=2)
        return nx.draw_networkx_nodes(G,pos,nodelist=["GOD"],node_size=300,node_color='r')