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
from itertools import product
from multiprocessing import Pool, Process
import subprocess
from datetime import datetime


# ---------- PARAMETERS -------------

nsizes = [2000]
homo_nbd = [0.25, 0.5, 0.75, 0.9]
homo_blk = [0.5, 0.75]
#nsizes = [100, 500, 1000, 1500, 3000]
#homos = [0.5, 0.8, 0.95]
NUM_RDS_SAMPLES = 200
MAX_RDS_SAMPLE_SIZE = 100
RDS_NUM_SEEDS = 5

# product generates a cartesian product
paramCombinations = list(product(nsizes, homo_nbd, homo_blk))
paramCombinations = paramCombinations[:]

# ------------ BEGIN CODE -------------


def pullRDSsamples( folder ):
    with open( path.join(folder, 'edges.csv') ) as ef:
        edges = list(DictReader(ef))
    with open( path.join(folder, 'nodes.csv') ) as nf:
        nodes = list(DictReader(nf))
        
    # we only care about the final result!
    maxt = max(nodes, key=lambda x: float(x['t']))['t']
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
        
        try:
            n.performRDS(numseeds=RDS_NUM_SEEDS, maxsample=MAX_RDS_SAMPLE_SIZE)
            n.RDS_CSV( path.join(folder, "RDSsample.%s.csv" % i) )
        except DeepRecursionException:
            print "Recursion has gone tooooo deep. Quitting for folder %s" % folder
            # abort the whole mission.
            # if we can't get an RDS sample in 100 tries, just quit!
            return

def now():
    return str(datetime.now())

def executeCommand(cmd):
    logFn = path.join( logdir, "log_%s.txt" % now() )
    
    print("command <<< %s >>> log <<< %s >>>" % (cmd, logFn))
    out = subprocess.check_output(cmd + " > %s 2> %s " % (logFn, logFn), shell=True)
    #open(logFn, 'w').write(out)
    # print(simOut)

if False:
    # this context manager only works in Python 3.3+
    # splits the application of the function across 4 cores :)
    with Pool(processes=4) as p:
        p.map( func=go, iterable=product(nsizes, homos), chunksize=1 )

print("Welcome to runEverything.py")
print("Working out of: %s" % os.getcwd())

# create the workspace. directory structure to be filled.
def mkRunDir(thisRuni):
    x = "sim_%s" % thisRuni
    return path.join('simulationRuns',  x)

thisRuni = 1
workingDirectory = mkRunDir(thisRuni)
while path.exists( workingDirectory ):
    thisRuni += 1
    workingDirectory = mkRunDir(thisRuni)
os.mkdir(workingDirectory)
print("Workspace directory: %s" % workingDirectory)

# subdirectories
print("Creating subdirectories")
scriptdir = path.join(workingDirectory, "scripts")
analysisdir = path.join(workingDirectory, "analysis")
datadir = path.join(workingDirectory, "data")
logdir = path.join(workingDirectory, "logs")

[ os.mkdir(x) for x in [
    scriptdir,
    analysisdir,
    datadir,
    logdir
] ]

# keep all scripts for later reference!
copy("episim/epidemicSimulation.R", scriptdir)
copy("episim/analysis.R", scriptdir)
copy("episim/runEverything.py", scriptdir)

# This script uses EpiModel to simulate an epidemic outbreak in a group of people
# What we know about the individuals are their relationships, after this guy is done, are:
#      + their networked relationships with eachother, for RDS sampling
#      + files detailing the spread of the epidemic and the population properties, throughout time
# 

def OutbreakAndRDS(params):
    
    def mkWorldDir(thisWorldi):
        x = "epi_%s_%s" % (
            "-".join(str(x) for x in params),
            thisWorldi
        )
        return path.join(datadir,  x)
    
    thisWorldi = 1
    thisWorldDir = mkWorldDir(thisWorldi)
    while path.exists( thisWorldDir ):
        thisWorldi += 1
        thisWorldDir = mkWorldDir(thisWorldi)
    os.mkdir(thisWorldDir)
    
    print("Epi model", params)
    
    print("Rendering epi graph")
    epiGraphCmd = "Rscript --vanilla episim/epidemicSimulation.R \"%s\" " % thisWorldDir
    epiGraphCmd += " ".join( '"%s"'%x for x in params )
    executeCommand(epiGraphCmd)
        
    # Using this data, we perform 200 RDS samples on the population
    print("..Pulling 200 RDS samples...")    

    pullRDSsamples( thisWorldDir )

# queue up all jobs across a maximum of 4 cores
print("Queueing network simulation processes") 
print("Putting %s of these in the queue in a few seconds, and then waiting forever" % len(paramCombinations))

parallel = True
if parallel:
    #THIS IS AWESOME!
    pool = Pool(processes=4) 
    pool.map_async( OutbreakAndRDS, paramCombinations )

    import time
    time.sleep( 1500000 )
else:
    for x in paramCombinations:
        OutbreakAndRDS(x)

# just do this somewhere else
    
#    # Analyze how this world did.    
#    print("Computing RDS statistics!")
#    executeCommand( "Rscript --vanilla analysis.R %s" % thisWorldDir )

# IT WON'T CONTINUE UNLESS WE JUST WAIT AND WAIT!    
