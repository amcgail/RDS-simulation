library(RDS)

N_RDS_SAMPLES <- 200

toEstimate <- list.dirs("data/ergm")

bigDf <- NULL

for( dr in toEstimate) {
  # for some reason this shows up...
  if( dr == "data/ergm" ) next;
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

for(pop in unique(bigDf$pop)) {
  toPlot <- c("testatus_i_rdsIest", "testatus_s_rdsIest", "testatus_0_rdsIest", "testatus_i_rdsIIest", "testatus_s_rdsIIest", "testatus_0_rdsIIest")
  myData <- bigDf[bigDf$pop == pop, c(toPlot, "testatus_i_true", "testatus_s_true", "testatus_0_true")]
  trPlot <- c("testatus_i_true", "testatus_s_true", "testatus_0_true", "testatus_i_true", "testatus_s_true", "testatus_0_true")
  
  par(mfrow=c(3,2))
  for(attri in range(length(toPlot))) {
    attr <- toPlot[attri]
    trValue <- myData[,trPlot[attri]]
    hist(myData[,attr], main=paste(pop, attr), breaks=15)
    abline(v=trValue, col="red")
  }
}