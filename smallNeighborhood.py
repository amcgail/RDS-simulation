# -*- coding: utf-8 -*-
"""
Created on Tue Apr 24 15:37:02 2018

@author: alec
"""

from lib import *

n = Network()
n.addPeople( 500 )

n.genOLSwage()
n.makeFriends()

n.performRDS()

n.full_CSV("data/smallNeighborhood.full.csv")
n.RDS_CSV("data/smallNeighborhood.csv")