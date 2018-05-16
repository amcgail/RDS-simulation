library(RDS)
library(plyr)
library(rmarkdown)
library(ggplot2)
library(igraph)

args <- commandArgs(trailingOnly = TRUE)

N_RDS_SAMPLES <- 500

basePath <- file.path('simulationRuns', args[1])
#basePath <- "simulationRuns/sim_6/"

imgDir <- file.path(basePath, "img")
dir.create( imgDir, showWarnings=F )

toEstimate <- list.dirs(file.path(basePath, "data"), recursive=F, full.names=T)

bigDfFn <- file.path( basePath, "RDSanalysis.csv" )


# generate a list of all columns I'll be interested in, along with their true values!

toPlot <- NULL
newcols <- outer("testatus_", c("i","s","r","0"), FUN="paste0")
newcols <- outer( paste0(newcols,"_"), c("rdsIest","rdsIIest"), FUN="paste0")
toPlot <- c(toPlot, newcols)

newcols <- outer("blk_", c("1","0"), FUN="paste0")
newcols <- outer( paste0(newcols,"_"), c("rdsIest","rdsIIest"), FUN="paste0")
toPlot <- c(toPlot, newcols)

trPlot <- NULL
newcols <- outer("testatus_", c("i","s","r","0"), FUN="paste0")
newcols <- outer( paste0(newcols,"_"), c("true","true"), FUN="paste0")
trPlot <- c(trPlot, newcols)

newcols <- outer("blk_", c("1","0"), FUN="paste0")
newcols <- outer( paste0(newcols,"_"), c("true","true"), FUN="paste0")
trPlot <- c(trPlot, newcols)

# global variables for debugging
lastReingold <- NULL
eIte <- NULL
d <- NULL

# if the full RDS analysis hasn't already been done, do that!
# generates reingold plots of RDS samples
if( file.exists(bigDfFn) ){
  bigDf <- read.csv(bigDfFn)
} else {
  # directory for reingold plots
  dir.create( file.path(imgDir,"reingold"), showWarnings=F )
  reingoldLimit = 15
  
  bigDf <- NULL
  
  for( dr in toEstimate) {
    if( !file.exists(file.path( dr, 'RDS.full.csv' )) )
      next;
    
    print(paste("Found file", "RDS.full.csv", "in dir", dr, "... analyzing"))
    
    d.full <- read.csv( file.path( dr, 'RDS.full.csv' ) )
    
    reingoldCount = 0
    
    for( i in 0:N_RDS_SAMPLES ) {
      sampleFn <- file.path( dr, paste("RDSsample", i, "csv", sep=".") )
      #print(sampleFn)
      if( (i+1) %% 50 == 0 ) {
        print(paste("On RDS sample", i-1))
      }
      
      if( !file.exists(sampleFn) ) {
        print( paste('Warning... Expected file', sampleFn, 'to exist...') )
        next;
      }
      
      d <<- read.csv( sampleFn, colClasses=c("integer", "integer", "integer", "character", "character") )
      # all seeds should be given the same unique identifier
      seeds <- d[ is.na( d$recruiter ), "recruit"]
      d[ d$recruit %in% seeds, "recruit" ] <- -1
      d[ d$recruiter %in% seeds, "recruiter" ] <- -1
      d <- d[ d$recruit != -1, ]
      # print(length(d[,1]))
      
      dfr <- list(i, dr)
      names(dfr) <- c("sampleNo", "pop")
      
      rds.df <- as.rds.data.frame(d, id = "recruit", network.size = "degree", population.size = NULL, max.coupons = 3, recruiter.id = "recruiter")
      cumulative.estimate(rds.df, outcome.variable = "blk")
      
      rdsEstimates <- function (attr) {
        # the names for this row
        fn <- NULL
        
        # the values for this row
        fr <- NULL
        
        # make the RDS computations
        eIte <<- RDS.I.estimates(rds.df, outcome.variable = attr)
        eIIte <- RDS.II.estimates(rds.df, outcome.variable = attr)
        
        # add the estimates for this RDS sample
        fn <- c(fn, paste(attr, "_", names(eIte$estimate), "_rdsIest", sep="") )
        fn <- c(fn, paste(attr, "_", names(eIIte$estimate), "_rdsIIest", sep="") )
        
        fr <- c(fr, eIte$estimate)
        fr <- c(fr, eIIte$estimate)
        
        # add the se for this RDS sample
        fn <- c(fn, paste(attr, "_", rownames(eIte$interval), "_rdsIse", sep="") )
        fn <- c(fn, paste(attr, "_", rownames(eIIte$interval), "_rdsIIse", sep="") )
        
        fr <- c(fr, eIte$interval[,"s.e."])
        fr <- c(fr, eIIte$interval[,"s.e."])
        
        fn <- c(fn, paste(attr, "_", rownames(eIte$interval), "_rdsI95low", sep="") )
        fn <- c(fn, paste(attr, "_", rownames(eIIte$interval), "_rdsII95low", sep="") )
        
        fr <- c(fr, eIte$interval[,"lower"])
        fr <- c(fr, eIIte$interval[,"lower"])
        
        fn <- c(fn, paste(attr, "_", rownames(eIte$interval), "_rdsI95high", sep="") )
        fn <- c(fn, paste(attr, "_", rownames(eIIte$interval), "_rdsII95high", sep="") )
        
        fr <- c(fr, eIte$interval[,"upper"])
        fr <- c(fr, eIIte$interval[,"upper"])
        
        true <- prop.table(table(d.full[,attr]))
        
        fn <- c(fn, paste(attr, "_", names(true), "_true", sep="") )
        fr <- c(fr, true)
        
        names(fr) <- fn
        fr
      }
      
      dfr <- c(
        dfr,
        rdsEstimates("testatus"),
        rdsEstimates("blk")
      )
      
      dfr <- as.data.frame(dfr)
      bigDf <- rbind.fill(bigDf, dfr)
      
      lastReingold <- rds.df
      thisImgDir <- file.path(dr, "img")
      reingoldDir <- file.path(thisImgDir, "reingold")
      dir.create( thisImgDir, showWarnings=F )
      dir.create( reingoldDir, showWarnings=F )
      
      if(reingoldCount < reingoldLimit) {
        rds.df$blk_shape = "circle"
        rds.df[rds.df$blk == 1,"blk_shape"] = "square"
        
        png( file.path(reingoldDir, paste(as.char(i),"png", sep=".")) )
        reingold.tilford.plot(
          rds.df, 
          vertex.color="testatus", 
          vertex.size=5, 
          #vertex.label="blk", 
          vertex.label=NA,
          vertex.label.cex=0.5, 
          vertex.shape=rds.df$blk_shape,
          show.legend=F,
          #plot=T
        )
        dev.off()
        
        reingoldCount <- reingoldCount + 1
      }
    }
  }
  
  write.csv( bigDf, bigDfFn )
}

if(T) {
  # high-level plot of 95% confidence intervals
  print(paste("Constructing 95% confidence interval coverage plot"))
  
  pops <- unique(bigDf$pop)
  highLevel <- ldply(pops, function(pop) {
    mySubset <- bigDf[bigDf$pop == pop,]
    
    stopifnot(length(unique(mySubset$testatus_i_true)) == 1)
    true <- unique(mySubset$testatus_i_true)[[1]]
    
    inCI <-  (mySubset$testatus_i_rdsI95low <= mySubset$testatus_i_true) & 
      (mySubset$testatus_i_rdsI95high >= mySubset$testatus_i_true)
    
    # print(length(mySubset[,1]))
    areIn <- table(inCI)["TRUE"]
    arentIn <- table(inCI)["FALSE"]
    
    percent <- areIn / (areIn + arentIn)
    
    popName <- basename(as.character(pop))
    
    data.frame(
      pop=pop,
      popName=popName,
      percent=percent,
      true=true
    )
    
  })
  
  write.csv(highLevel, file.path(basePath, "highLevel.csv"))
  
  ggplot(highLevel, aes(x=popName, y=percent)) +
    geom_point() +
    labs(
      x="Population Name",
      y="Coverage",
      title="Coverage of estimated 95% confidence interval"
    ) +
    geom_hline(yintercept = 0.95) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    #annotate("text", min(the.data$year), 50, vjust = -1, label = "Test") +
    scale_linetype_manual("solid")
  ggsave( file.path(imgDir,paste("95confCoverage","png",sep=".")) )
}

if(F) {
  # render adjacency plots:
  # Create the adjacency matrix plot
  # These look so freaking bad.
  # Not worth it at all, and not useful in plotting a network
  
  for( dr in toEstimate ) {
    edges <- read.csv( file.path( dr, 'edges.csv' ) )
    nodes <- read.csv( file.path( dr, 'nodes.csv' ) )
    
    maxt <- max(unique(edges$t))
    
    edges <- edges[edges$sim_i == 1 & edges$t == maxt, c("out","in.")]
    nodes <- nodes[nodes$sim_i == 1 & nodes$t == maxt,c("person","blk")]
    
    graph <- graph.data.frame(edges, directed=F, vertices = nodes)
  
    # Create a character vector containing every node name
    all_nodes <- sort(nodes$person)
    
    # Adjust the 'to' and 'from' factor levels so they are equal
    # to this complete list of node names
    plot_data <- edges %>% mutate(
      out = factor(out, levels = all_nodes),
      in. = factor(in., levels = all_nodes))
    
    ggplot(plot_data, aes(x = out, y = in.)) +
      geom_raster() +
      theme_bw()
      # Because we need the x and y axis to display every node,
      # not just the nodes that have connections to each other,
      # make sure that ggplot does not drop unused factor levels
      #scale_x_discrete(drop = FALSE) +
      #scale_y_discrete(drop = FALSE) +
      #theme(
        # Rotate the x-axis lables so they are legible
      #  axis.text.x = element_text(angle = 270, hjust = 0),
        # Force the plot into a square aspect ratio
      #  aspect.ratio = 1,
        # Hide the legend (optional)
      #  legend.position = "none")
  }
}

if(T) {
  # render summary pdfs!
  for( dr in toEstimate ) {
    d.full <- read.csv( file.path( dr, 'RDS.full.csv' ) )
    rmarkdown::render("episim/summaryStats.Rmd", params=list(
      basePath=dr
    ), output_file = file.path( "..", dr, "img", 'summaryStats.pdf'))
  }
}
  
if(T) {
  dir.create( file.path(imgDir,"ests"), showWarnings=F )
  # these plots are nice...
  for(pop in unique(bigDf$pop)) {
    myData <- bigDf[bigDf$pop == pop, ] #c(toPlot, "testatus_i_true", "testatus_s_true", "testatus_0_true")
    
    for(attri in 1:length(toPlot)) {
      if( !( toPlot[attri] %in% names(myData)) ) {
        next;
      }
      attr <- toPlot[attri]
      
      png( file.path(imgDir, "ests", paste(basename(pop),attr,"png",sep=".")) )
      
      trValue <- myData[,trPlot[attri]]
      hist(myData[,attr], main=paste(pop, attr), breaks=20)
      abline(v=trValue, col="red")
      
      dev.off()
    }
  }
}

if(F) {
  library(igraph)
  library(intergraph)
  
  n.ig <- asIgraph(n)
  
  V(n.ig)$frame.color <- "white"
  V(n.ig)$color <- "orange"
  V(n.ig)$label <- ""
  V(n.ig)$size <- 10
  E(n.ig)$arrow.mode <- 0
  
  n.sub <- induced_subgraph(graph=n.ig, vids=sample(V(n.ig), size=200))
  plot(n.sub)
}


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


if(F) {
  reingold.tilford.plot(
    rds.df, 
    vertex.color="testatus", 
    vertex.size=5, 
    #vertex.label="blk", 
    vertex.label=NA,
    vertex.label.cex=0.5, 
    vertex.shape=rds.df$blk_shape,
    show.legend=F,
    #plot=T
  )
}