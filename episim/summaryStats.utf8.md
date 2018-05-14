---
Title: Summary Statistics
output: pdf_document
params:
  basePath: ""
---




```r
# paste("working out of directory", as.char(basePath))
# indir <- file.path(basePath, 'RDS.full.csv' )
# d.full <- read.csv(indir)

table(d.full$blk)
```

```
## 
##    0    1 
##  968 1032
```

```r
table(d.full$testatus)
```

```
## 
##    i    s 
##  411 1589
```

```r
hist(d.full$degree)
```

![](../simulationRuns/sim_6//data/epi_2000-0.25-0.5_1/img/summaryStats_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 
