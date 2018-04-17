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

import networkx as nx
from random import sample
from itertools import chain

g = nx.fast_gnp_random_graph(50, 0.5)
nx.set_node_attributes(g, name="sampled", values={x:False for x in nx.nodes(g)})
ns = list(g.nodes)

def RDS_sample(g, n, rd=0):
    if rd > 10:
        raise
    neigh = list(g[n])
    valid = [ x for x in neigh if not g.node[x]['sampled'] ]
    ppl = sample(valid, min(len(valid), 3))
    
    if not len(ppl):
        return []
        
    for x in ppl:
        g.node[x]['sampled'] = True
    
    return [ (n,p) for p in ppl ] + list(chain.from_iterable( RDS_sample(g, x, rd+1) for x in ppl ))
    
sample = RDS_sample(g, 0)