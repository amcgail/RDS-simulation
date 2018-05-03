# -*- coding: utf-8 -*-
"""
Created on Wed May  2 22:25:54 2018

@author: alec
"""

import os
from lib import *
from csv import DictReader
from os import path

NUM_RDS_SAMPLES = 200

def pullRDSsamples( folder ):
    with open( path.join(folder, 'edges.csv') ) as ef:
        edges = list(DictReader(ef))
    with open( path.join(folder, 'nodes.csv') ) as nf:
        nodes = list(DictReader(nf))
        
    # we only care about the final result!
    maxt = max(nodes, key=lambda x: int(x['t']))['t']
    print(maxt)
    
    edges = filter( lambda x: x['sim_i'] == '1' and x['t'] == maxt, edges )
    nodes = filter( lambda x: x['sim_i'] == '1' and x['t'] == maxt, nodes )
    
    print(len(nodes), "nodes found")
    
    for i in range(NUM_RDS_SAMPLES):
        print( "RDS SAMPLE %s" % i )
        
        n = Network()
        
        for pinfo in nodes:
            p = Person(pinfo['person'])
            p['blk'] = pinfo['blk']
            #p['nbd'] = pinfo['nbd']
            p['testatus'] = pinfo['testatus']
            n.addPerson( p )
            
        for e in edges:
            f = e['out']
            t = e['in']
            n.pdict[f].addFriend( n.pdict[t] )
        
        n.performRDS(numseeds=10, maxsample=150)
        n.RDS_CSV( path.join(folder, "RDSsample.%s.csv" % i) )

nsizes = [100, 500, 1000, 1500, 3000]
homos = [0.5, 0.8, 0.95]

for nsize in nsizes:
    for homo in homos:
        params = (nsize, homo)
        print(params)
        
        os.system("Rscript --vanilla networkSimulation.alec.R %s %s" % (nsize, homo))
        
        folder = path.join('data','ergm','%s-%s' % params)
        
        pullRDSsamples( folder )