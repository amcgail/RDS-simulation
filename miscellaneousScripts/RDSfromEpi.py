from lib import *
from csv import DictReader

with open( 'data/ergm/edges.csv' ) as ef:
    edges = list(DictReader(ef))
with open( 'data/ergm/nodes.csv' ) as nf:
    nodes = list(DictReader(nf))

edges = filter( lambda x: x['sim_i'] == '1' and x['t'] == '21', edges )
nodes = filter( lambda x: x['sim_i'] == '1' and x['t'] == '21', nodes )

n = Network()

for pinfo in nodes:
    p = Person(pinfo['person'])
    p['blk'] = pinfo['blk']
    p['testatus'] = pinfo['testatus']
    n.addPerson( p )
    
for e in edges:
    f = e['out']
    t = e['in']
    n.pdict[f].addFriend( n.pdict[t] )

n.performRDS(numseeds=10, maxsample=350)
n.RDS_CSV("data/ergm/RDSsample.RDS.csv")
n.full_CSV("data/ergm/RDSsample.full.csv")