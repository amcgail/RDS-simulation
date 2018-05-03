# Task: Simulate SIS graphs with RDS estimates and sensitivity analyses
# Authors: Alexander Ruch and Alec McGail
# Refs: http://www.epimodel.org/tut.html
# Last updated: 4/25/2018

################################################################################
# Set working directory
# setwd("/Users/amruch/Documents/Research/~ Projects/Cornell Research/RDS-simulation")


NSTEPS = 50
NSIMULATIONS = 1

args <- commandArgs(trailingOnly = TRUE)

fn <- args[1]

# delete first arg
args <- args[-1]

folder <- paste( fn, paste( args, collapse="-" ), sep="/" )

dir.create(folder)

nodeFn <- paste( folder, "nodes.csv", sep="/" )
edgeFn <- paste( folder, "edges.csv", sep="/" )

# ------maybe 500, 1000, 1500, 3000?
nnodes = as.double( args[1] )
ninfected = 10
meandeg = 3
nedges = nnodes*meandeg
# mindeg = 1
# maxdeg = 14
# ------maybe 0.5, 0.8, 0.95 ?
homo = as.double( args[2] )
infectionProb = 0.05


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

################################################################################
# AGING MODULE:
## At time step 1, actors are randomly assigned an age between 18 and 49 years
## At subsequent time steps, their age is incremented by one month
afunc <- function(dat, at) {
    
    ## Attributes
    if (at == 2) {
        n <- sum(dat$attr$active == 1)
        dat$attr$age <- sample(18:49, n, replace = TRUE)
    } else {
        dat$attr$age <- dat$attr$age + 1/12
    }
    
    ## Summary statistics
    if (at == 2) {
        dat$epi$meanAge <- rep(mean(dat$attr$age, na.rm = TRUE), 2)
    } else {
        dat$epi$meanAge[at] <- mean(dat$attr$age, na.rm = TRUE)
    }
    
    return(dat)
}

# DEATH MODULE:
## Make death a non-linear function of age with life expectancy a variable parameter
## Set the death rate calculation in the module function
## Who dies at any time step will be based on random draws from binomial
##   distributions which probabilities equal to their age-specific risks
ages <- 18:49
death.rates <- 1/(70*12 - ages*12)
#par(mar = c(3.2, 3.2, 1, 1), mgp = c(2, 1, 0))
#plot(ages, death.rates, pch = 20, xlab = "age", ylab = "Death Risk")

dfunc <- function(dat, at) {
    
    # Parameters
    idsElig <- which(dat$attr$active == 1)
    nElig <- length(idsElig)
    nDeaths <- 0
    
    # Processes
    if (nElig > 0) {
        ages <- dat$attr$age[idsElig]
        life.expt <- dat$param$life.expt
        death.rates <- pmin(1, 1/(life.expt*12 - ages*12))
        vecDeaths <- which(rbinom(nElig, 1, death.rates) == 1)
        idsDeaths <- idsElig[vecDeaths]
        nDeaths <- length(idsDeaths)
        
        # Update nodal attributes on attr and networkDynamic object
        if (nDeaths > 0) {
            dat$attr$active[idsDeaths] <- 0
            dat$attr$exitTime[idsDeaths] <- at
            dat$nw <- deactivate.vertices(dat$nw, onset = at, terminus = Inf, 
                                          v = idsDeaths, deactivate.edges = TRUE)
        }
    }
    
    # Summary statistics
    if (at == 2) {
        dat$epi$d.flow <- c(0, nDeaths)
    } else {
        dat$epi$d.flow[at] <- nDeaths
    }
    
    return(dat)
}

# BIRTH MODULE:
## Simulate number of new births at each time step as a function of a fixed rate
## Population expected to grow linearly at each time step at a fixed growth rate
## Adds new nodes to network with attributes of active, susceptible, no infection
##   time, entry time equals `at`, no exit time, and enter population at age 18
bfunc <- function(dat, at) {
    
    # Variables
    growth.rate <- dat$param$growth.rate
    exptPopSize <- dat$epi$num[1] * (1 + growth.rate * at)
    n <- network.size(dat$nw)
    tea.status <- dat$control$tea.status
    
    numNeeded <- exptPopSize - sum(dat$attr$active == 1) 
    if (numNeeded > 0) {
        nBirths <- rpois(1, numNeeded)
    } else {
        nBirths <- 0
    }
    if (nBirths > 0) {    
        dat$nw <- add.vertices(dat$nw, nv = nBirths)
        newNodes <- (n + 1):(n + nBirths)
        dat$nw <- activate.vertices(dat$nw, onset = at, terminus = Inf, v = newNodes)
        set.vertex.attribute(dat$nw, "blk", rbinom(nBirths, 1, 0.5), v=newNodes)
        set.vertex.attribute(dat$nw, "nbd", sample(1:10, network.size(nw), replace=T), v=newNodes)
    }
    
    # Update attributes
    if (nBirths > 0) {
      
        dat$attr$active <- c(dat$attr$active, rep(1, nBirths))
        dat$attr$status <- c(dat$attr$status, rep("s", nBirths))  
        dat$attr$infTime <- c(dat$attr$infTime, rep(NA, nBirths))
        dat$attr$entrTime <- c(dat$attr$entrTime, rep(at, nBirths))
        dat$attr$exitTime <- c(dat$attr$exitTime, rep(NA, nBirths))
        dat$attr$age <- c(dat$attr$age, rep(18, nBirths))
        if (tea.status == TRUE) {
            dat$nw <- activate.vertex.attribute(dat$nw, prefix = "testatus", 
                                                value = 0, onset = at, 
                                                terminus = Inf, v = newNodes)
        }
    }
    
    # Summary statistics
    if (at == 2) {
        dat$epi$b.flow <- c(0, nBirths)
    } else {
        dat$epi$b.flow[at] <- nBirths
    }
    
    return(dat)
}

demographicInit <- function(dat, at) {
  
}

# NETWORK MODEL PARAMETERIZATION:
## Fit a basic, undirected, random-mixing model for our ERGM
nw <- network.initialize(nnodes, directed = FALSE)

n_nbds <- 10

# set demographics
set.vertex.attribute(nw, "blk", rbinom(network.size(nw), 1, 0.5))
set.vertex.attribute(nw, "nbd", sample(1:n_nbds, network.size(nw), replace=T))

homo_blk <- homo * nedges + nedges / 2
homo_nbd <- homo * nedges + nedges / n_nbds

formation <- ~edges + nodematch("blk") + nodematch("nbd")
target.stats <- c(nedges, homo_blk, homo_nbd)

# are we actually getting anything better from ERGM!? it doesn't feel like it...
if(F) {
  spec <- nw ~ edges + nodematch('blk') + nodematch('nbd')
  target.stats <- c(nedges, nedges*homo, nedges*homo)
  
  fit <- ergm(spec, target.stats = target.stats)
  sim <- simulate(fit)
  plot(sim, vertex.col="blk", cex=.1)
  plot(sim, vertex.col="nbd", vertex.cex=.3)
  
  mixingmatrix(sim, "nbd")
  
  plot(degreedist(sim))
}
  
est <- netest(nw, formation, target.stats, coef.diss = dissolution_coefs(~offset(edges), 60, mean(death.rates)))

# EPIDEMIC MODEL PARAMETERIZATION:
## Collect newly created modules but leave transmission modules unchanged
## Must consider inputs and outputs needed for or generated by modules
## Use same infection probability and act rate as built-in models
## To add more modules, give the module a name not among those modules available
##   for replacement; module name must end in .FUN to register as a module
## Model type, # simulations, and time steps per simulation set in control.net
## Replacement modules in each time step are run in order listed in control.net
## Module order may be set by module.order argument in control.net
param <- param.net( inf.prob = infectionProb, growth.rate = 0.00083, life.expt = 70 )
init <- init.net(i.num = ninfected)

control <- control.net(type = "SI", nsims = NSIMULATIONS, nsteps = NSTEPS,
                       deaths.FUN = dfunc, births.FUN = bfunc, aging.FUN = afunc, 
                       depend = TRUE, save.network = TRUE)

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