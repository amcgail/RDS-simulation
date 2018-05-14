# Task: Simulate SIS graphs with RDS estimates and sensitivity analyses
# Authors: Alexander Ruch and Alec McGail
# Refs: http://www.epimodel.org/tut.html
# Last updated: 4/25/2018

################################################################################
# Set working directory
# setwd("/Users/amruch/Documents/Research/~ Projects/Cornell Research/RDS-simulation")

args <- commandArgs(trailingOnly = TRUE)

folder <- args[1]

# delete first arg
args <- args[-1]

if( !dir.exists(folder) ) {dir.create(folder)}

nodeFn <- paste( folder, "nodes.csv", sep="/" )
edgeFn <- paste( folder, "edges.csv", sep="/" )

# ----------- ARGUMENTS -------------

NSIMULATIONS = 2

nnodes = as.double( args[1] )
homo_blk = as.double( args[2] )
homo_nbd = as.double( args[3] )

meandeg = 2
nedges = nnodes*meandeg
# mindeg = 1
# maxdeg = 14
# ------maybe 0.5, 0.8, 0.95 ?

ninfected = 10
n_months = 12 * 2 # 12 * 5
NSTEPS = n_months
OUTPUT_TIME_RESOLUTION = 6 # output a network every six months

n_nbds <- 3
p_blk <- 0.5

# parameters of epidemic spread
rec.rate = 1 / 6

# average number of transmissible acts per partnership per unit time
act.rate = 8
#act.rate = 4

if(F) {
  r_0 = 2
  inf.prob = r_0 * rec.rate / act.rate
}

inf.prob = 0.0067

# death rate among the population (how often they die)
d.rate = 1 / (4*100)

# ----------- LIBRARIES -----------

# install.packages(c("deSolve", "tergm", "ergm", "ape", "ergm.count"))
#install.packages(c("statnet","NetSim","EpiModel","RDS"))

# Import libraries
toLoad <- c(
  "statnet",
  "NetSim",
  "EpiModel",
  "RDS",
  "network",
  "plyr"
)
loadLibrary <- function(x) {
  library(x, quietly = TRUE);
}
lapply( toLoad, require, character.only = TRUE );

# NETWORK MODEL PARAMETERIZATION:
## Fit a basic, undirected, random-mixing model for our ERGM
nw <- network.initialize(nnodes, directed = FALSE)

# set demographics
set.vertex.attribute(nw, "blk", rbinom(network.size(nw), 1, p_blk))
set.vertex.attribute(nw, "nbd", sample(1:n_nbds, network.size(nw), replace=T))

blk_same_edges <- nedges * ( homo_blk + (1-homo_blk) * 1/2 )
nbd_same_edges <- nedges * ( homo_nbd + (1-homo_nbd) * 1/n_nbds )

formation <- ~edges + nodematch("blk") + nodematch("nbd")
target.stats <- c(nedges, blk_same_edges, nbd_same_edges)
  
est <- netest(nw, formation, target.stats, coef.diss = dissolution_coefs(~offset(edges), 12, d.rate))

param <- param.net( 
  inf.prob = inf.prob, 
  act.rate = act.rate,
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

# export diagnostic plots :)

imgDir <- file.path( folder, "img" )
if( !dir.exists(imgDir) ) {
  dir.create( imgDir )
}
png( file.path(imgDir, "epidemicSummary.png"), width=900, height=450 )

par(mfrow = c(1,2))
plot(mod, main = "State Prevalences")
plot(mod, main = "State Prevalences", sim.lines=T)

dev.off()

# export network to CSV
nodesCSV <- ldply( 1:NSIMULATIONS, function(si) {
  ldply( seq(1,NSTEPS,OUTPUT_TIME_RESOLUTION), function(t) {
    n <- get_network(mod, sim = si, collapse = TRUE, at = t)
    ldply( 1:length(n$val), function(vi) {
      c( si, t, vi, unlist( n$val[[vi]] )[c("blk","testatus")] )
    } )
  })
})
names(nodesCSV) <- c("sim_i","t","person","blk","testatus")

edgesCSV <- ldply( 1:NSIMULATIONS, function(si) {
  ldply( seq(1,NSTEPS,OUTPUT_TIME_RESOLUTION), function(t) {
    n <- get_network(mod, sim = si, collapse = TRUE, at = t)
    ldply( 1:length(n$mel), function(ei) {
      c( si, t, n$mel[[ei]]$outl, n$mel[[ei]]$inl )
    } )
  })
})
names(edgesCSV) <- c("sim_i","t","out","in")
  
write.csv(nodesCSV, nodeFn)
write.csv(edgesCSV, edgeFn)