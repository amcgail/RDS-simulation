from lib import *

kedges = []
with open( 'data/kronecker_graph.txt' ) as kfile:
    for l in kfile:
        ls = l.split()
        if len(ls) != 2:
            continue
        kedges.append( map( int, ls ) )

n = Network.fromEdgeList( kedges )

#for i in range(100):
#    n.performRDS()

n.genOLSwage()
n.performRDS(numseeds=10, maxsample=1000)
n.exportCSV("data/kronecker.RDSsample.csv")