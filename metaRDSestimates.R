library(RDS)
library(plyr)

args <- commandArgs(trailingOnly = TRUE)

N_RDS_SAMPLES <- 200

basePath <- args[1]
toEstimate <- list.dirs(basePath)

bigDf <- NULL

for( dr in toEstimate) {
  # for some reason this shows up...
  if( dr == basePath ) next;
  d.full <- read.csv( file.path( dr, 'RDS.full.csv' ) )
  
  for( i in 0:N_RDS_SAMPLES ) {
    print(paste(dr, i))
    sampleFn <- file.path( dr, paste("RDSsample", i, "csv", sep=".") )
    if( !file.exists(sampleFn) ) {
      print( c('Warning... Expected file ', sampleFn, ' to exist...') )
      next;
    }
    
    d <- read.csv( sampleFn )
    # all seeds should be given the same unique identifier
    seeds <- d[ is.na( d$recruiter ), "recruit"]
    d[ d$recruit %in% seeds, "recruit" ] <- -1
    d[ d$recruiter %in% seeds, "recruiter" ] <- -1
    d <- d[ d$recruit != -1, ]
    
    dfr <- list(i, dr)
    names(dfr) <- c("sampleNo", "pop")
    
    rds.df <- as.rds.data.frame(d, id = "recruit", network.size = "degree", population.size = 200000, max.coupons = 3, recruiter.id = "recruiter")
    cumulative.estimate(rds.df, outcome.variable = "blk")
    
    rdsEstimates <- function (attr) {
      fn <- NULL
      fr <- NULL
      
      eIte <- RDS.I.estimates(rds.df, outcome.variable = attr)
      eIIte <- RDS.II.estimates(rds.df, outcome.variable = attr)
      
      fn <- c(fn, paste(attr, "_", names(eIte$estimate), "_rdsIest", sep="") )
      fn <- c(fn, paste(attr, "_", names(eIIte$estimate), "_rdsIIest", sep="") )
      
      fr <- c(fr, eIte$estimate)
      fr <- c(fr, eIIte$estimate)
      
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
    
    #reingold.tilford.plot(rds.df)
  }
}

if(T) {
  dir.create( file.path(basePath, "img") )
  
  # these plots are nice...
  for(pop in unique(bigDf$pop)) {
    toPlot <- c("testatus_i_rdsIest", "testatus_s_rdsIest", "testatus_0_rdsIest", "testatus_i_rdsIIest", "testatus_s_rdsIIest", "testatus_0_rdsIIest")
    myData <- bigDf[bigDf$pop == pop, c(toPlot, "testatus_i_true", "testatus_s_true", "testatus_0_true")]
    trPlot <- c("testatus_i_true", "testatus_s_true", "testatus_0_true", "testatus_i_true", "testatus_s_true", "testatus_0_true")
    
    for(attri in range(length(toPlot))) {
      attr <- toPlot[attri]
      
      png( file.path(args[1], "img", paste(basename(pop),".",attr,".png")) )
      
      trValue <- myData[,trPlot[attri]]
      hist(myData[,attr], main=paste(pop, attr), breaks=20)
      abline(v=trValue, col="red")
      
      dev.off()
    }
  }
  
  library(rmarkdown)
  rmarkdown::render("summaryStats.Rmd", params=list(
    basePath=basePath
  ))
}

write.csv( bigDf, file.path( args[1], "RDSanalysis.csv" ) )

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