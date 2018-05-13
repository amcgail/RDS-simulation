# Task: Simulate SIS graphs with RDS estimates and sensitivity analyses
# Authors: Alexander Ruch and Alec McGail
# Refs: http://www.epimodel.org/tut.html
# Last updated: 4/25/2018

################################################################################
# Set working directory
# setwd("/Users/amruch/Documents/Research/~ Projects/Cornell Research/RDS-simulation")

args <- commandArgs(trailingOnly = TRUE)

fn <- args[1]

# delete first arg
args <- args[-1]

folder <- paste( fn, paste( args, collapse="-" ), sep="/" )

dir.create(folder)

nodeFn <- paste( folder, "nodes.csv", sep="/" )
edgeFn <- paste( folder, "edges.csv", sep="/" )

# ----------- ARGUMENTS -------------

NSIMULATIONS = 1

nnodes = as.double( args[1] )
homo = as.double( args[2] )

meandeg = 2
nedges = nnodes*meandeg
# mindeg = 1
# maxdeg = 14
# ------maybe 0.5, 0.8, 0.95 ?

ninfected = 10
n_months = 12 * 5
NSTEPS = n_months

n_nbds <- 3
p_blk <- 0.5

rec.rate = 1 / 6
act.rate = 4
# r_0 = 2
# inf.prob = r_0 * rec.rate / act.rate
inf.prob = 0.0067

# what exactly does this measure...?
d.rate = 1 / (2*12)

# ----------- LIBRARIES -----------

# install.packages(c("deSolve", "tergm", "ergm", "ape", "ergm.count"))
#install.packages(c("statnet","NetSim","EpiModel","RDS"))

# Import libraries
library(statnet)
library(NetSim)
library(EpiModel)
#library(networkDynamicData)
library(RDS)
#library(igraph)
#library(sspse)
library(network)

# NETWORK MODEL PARAMETERIZATION:
## Fit a basic, undirected, random-mixing model for our ERGM
nw <- network.initialize(nnodes, directed = FALSE)

# set demographics
set.vertex.attribute(nw, "blk", rbinom(network.size(nw), 1, p_blk))
set.vertex.attribute(nw, "nbd", sample(1:n_nbds, network.size(nw), replace=T))

homo_blk <- nedges * ( homo + (1-homo) * 1/2 )
homo_nbd <- nedges * ( homo + (1-homo) * 1/n_nbds )

formation <- ~edges + nodematch("blk") + nodematch("nbd")
target.stats <- c(nedges, homo_blk, homo_nbd)
  
est <- netest(nw, formation, target.stats, coef.diss = dissolution_coefs(~offset(edges), 12, d.rate))

param <- param.net( 
  inf.prob = inf.prob, 
  rec.rate = rec.rate 
)

init <- init.net(
  i.num = ninfected,
  r.num = 0
)

control <- control.net(
  type = "SI", 
  nsims = NSIMULATIONS, 
  nsteps = n_months, 
  depend = TRUE, 
  save.network = TRUE
)

# RESULTS:
## Simulated with network object, parameters, initial conditions, and control.net
## Summary statistics in epi ending in .num automatically compartments and .flow as flows
mod <- netsim(est, param, init, control)
mod

# export network to CSV
library(plyr)
nodesCSV <- ldply( 1:NSIMULATIONS, function(si) {
  ldply( seq(1,NSTEPS,NSTEPS/5), function(t) {
    n <- get_network(mod, sim = si, collapse = TRUE, at = t)
    ldply( 1:length(n$val), function(vi) {
      c( si, t, vi, unlist( n$val[[vi]] )[c("blk","testatus")] )
    } )
  })
})
names(nodesCSV) <- c("sim_i","t","person","blk","testatus")

edgesCSV <- ldply( 1:NSIMULATIONS, function(si) {
  ldply( seq(1,NSTEPS,NSTEPS/5), function(t) {
    n <- get_network(mod, sim = si, collapse = TRUE, at = t)
    ldply( 1:length(n$mel), function(ei) {
      c( si, t, n$mel[[ei]]$outl, n$mel[[ei]]$inl )
    } )
  })
})
names(edgesCSV) <- c("sim_i","t","out","in")

if(F) {
  # basic attributes of prevalence
  par(mfrow = c(1,2))
  plot(mod, main = "State Prevalences")
  plot(mod, popfrac = FALSE, main = "State Sizes", sim.lines = TRUE, 
       qnts = FALSE, mean.smooth = FALSE)
  
  par(mfrow = c(1, 2))
  plot(mod, y = "num", popfrac = FALSE, main = "Population Size", ylim = c(0, 1000))
  plot(mod, y = "meanAge", main = "Mean Age", ylim = c(18, 70))
  
  par(mfrow = c(1, 2))
  plot(mod, y = "d.flow", popfrac = FALSE, mean.smooth = TRUE, qnts = 1, main = "Deaths")
  plot(mod, y = "b.flow", popfrac = FALSE, mean.smooth = TRUE, qnts = 1, main = "Births")
}

if(F) {
  # plot the network
  n <- get_network(mod, sim = 1, collapse = TRUE, at = 25)
  
  set.vertex.attribute(n, "testatus", nodesCSV[nodesCSV$t==41, "testatus"])
  
  plot(n, vertex.col="blk")
  plot(n, vertex.col="testatus", vertex.cex=.5, edge.cex=0)
}
  
write.csv(nodesCSV, nodeFn)
write.csv(edgesCSV, edgeFn)