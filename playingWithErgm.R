# create the ERGM network

# Task: Simulate SIS graphs with RDS estimates and sensitivity analyses
# Authors: Alexander Ruch and Alec McGail
# Refs: http://www.epimodel.org/tut.html
# Last updated: 4/25/2018

################################################################################
# Set working directory
# setwd("/Users/amruch/Documents/Research/~ Projects/Cornell Research/RDS-simulation")

# delete first arg
args <- c(100, 0.7)

# ------maybe 500, 1000, 1500, 3000?
nnodes = as.double( args[1] )
ninfected = 10
meandeg = 5
nedges = nnodes*meandeg
# mindeg = 1
# maxdeg = 14
# ------maybe 0.5, 0.8, 0.95 ?
homo = as.double( args[2] )
infectionProb = 0.05
NSTEPS = 50
NSIMULATIONS = 1
n_nbds <- 6


# install.packages(c("deSolve", "tergm", "ergm", "ape", "ergm.count"))
#install.packages(c("statnet","NetSim","EpiModel","RDS"))

# Import libraries
library(network)
library(ergm)
library(sna)
library(intergraph)
library(plyr)


# NETWORK MODEL PARAMETERIZATION:
## Fit a basic, undirected, random-mixing model for our ERGM
nw <- network.initialize(nnodes, directed = FALSE)

# set demographics
set.vertex.attribute(nw, "blk", rbinom(network.size(nw), 1, 0.5))
set.vertex.attribute(nw, "nbd", sample(1:n_nbds, network.size(nw), replace=T))

homo_blk <- nedges * ( homo + (1-homo) * 1/2 )
homo_nbd <- nedges * ( homo + (1-homo) * 1/n_nbds )

formation <- ~edges + nodematch("blk") + nodematch("nbd")
target.stats <- c(nedges, homo_blk, homo_nbd)

# now generate an ERGM graph and assess whether it's doing a good job..

spec <- nw ~ edges + nodematch('blk') + nodematch('nbd')
target.stats <- c(nedges, homo_blk, homo_nbd)

fit <- ergm(spec, target.stats = target.stats)


if(F) {
  library(igraph)
  
  stats = NULL
  
  for( i in 1:100 ) {
    sim <- simulate(fit)
    plot(sim, vertex.col="blk")
    plot(sim, vertex.col="nbd")
    plot(degreedist(sim))
    
    ig <- asIgraph(sim)
    
    
    edges <- as.data.frame(ends(ig, E(ig)))
    edges$blk1 <- get.vertex.attribute(ig, "blk", edges$V1)
    edges$blk2 <- get.vertex.attribute(ig, "blk", edges$V2)
    edges$nbd1 <- get.vertex.attribute(ig, "nbd", edges$V1)
    edges$nbd2 <- get.vertex.attribute(ig, "nbd", edges$V2)
    
    s1 <- sum(edges$blk1 == edges$blk2)
    s2 <- sum(edges$nbd1 == edges$nbd2)
    
    nr <- data.frame(
      blk=s1,
      nbd=s2
    )
    
    stats <- rbind.fill(stats, nr)
  }
}