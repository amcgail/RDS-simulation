library(RDS)
d <- read.csv('test.csv')

# all seeds should be given the same unique identifier
seeds <- d[ is.na( d$recruiter ), "recruit"]
d[ d$recruit %in% seeds, "recruit" ] <- -1
d[ d$recruiter %in% seeds, "recruiter" ] <- -1
d <- d[ d$recruit != -1, ]

rds.df <- as.rds.data.frame(d, id = "recruit", network.size = "degree", population.size = 200000, max.coupons = 3, recruiter.id = "recruiter")
cumulative.estimate(rds.df, outcome.variable = "black")

RDS.I.estimates(rds.df, outcome.variable = "black")
RDS.II.estimates(rds.df, outcome.variable = "black")
reingold.tilford.plot(rds.df)