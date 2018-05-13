# -*- coding: utf-8 -*-
"""
Created on Wed May  2 22:25:54 2018

@author: alec
"""

import os
from lib import *
from csv import DictReader
from os import path
from shutil import copy

outdir = path.join('data',"pinf=0.05_50steps_3nbd")
scriptdir = path.join(outdir, "scripts")

os.mkdir(outdir)
os.mkdir(scriptdir)

# keep these for later reference!
copy("networkSimulation.alec.R", scriptdir)
copy("metaRDSestimates.R", scriptdir)
copy("metaScript.py", scriptdir)

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
        if i % 50 == 0:
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

        # at this point the networks will all be the same...
        #                                                                                                   so just record the whole things for future analysis
        #   (includes degree, etc. readable by my RDSestimates code)            
        if i == 0:
            n.full_CSV( path.join(folder, "RDS.full.csv") )
        
        n.performRDS(numseeds=10, maxsample=300)
        n.RDS_CSV( path.join(folder, "RDSsample.%s.csv" % i) )

#nsizes = [100, 500, 1000, 1500, 3000]
#homos = [0.5, 0.8, 0.95]

nsizes = [1000, 2000, 3000]
homos = [0, 0.25, 0.5, 0.75]

# we're going to try and parellelize this!
# this code is sort of cool

from itertools import product
from multiprocessing import Pool

def runEverything(params):
    print("Epi model", params)
    
    print("..Rendering epi graph...")
    os.system("Rscript --vanilla networkSimulation.alec.R %s %s %s" % tuple( [outdir] + list(params)))
    
    folder = path.join(outdir,'%s-%s' % params)
    

    print("..Pulling 200 RDS samples...")
    pullRDSsamples( folder )

# product generates a cartesian product
# this context manager only works in Python 3.3+
# splits the application of the function across 4 cores :)
with Pool(processes=4) as p:
    p.map( func=runEverything, iterable=product(nsizes, homos), chunksize=1 )
        

print(" now that everything is pulled...")
print(" let's compute RDS statistics!")

os.system("Rscript --vanilla metaRDSestimates.R %s" % outdir)
