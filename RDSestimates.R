library(RDS)
#d <- read.csv('data/kronecker.RDSsample.csv')
#d <- read.csv('data/smallNeighborhood.full.csv')
d.full <- read.csv('data/ergm/RDSsample.full.csv')
d <- read.csv('data/ergm/RDSsample.RDS.csv')

# interesting. can I get the OLS back?
#d$educblack = d$educ*d$black
#d$lnwage = log(d$wage)
#l <- lm(lnwage ~ black + educ + educblack + ability, d)
# yes...

# all seeds should be given the same unique identifier
seeds <- d[ is.na( d$recruiter ), "recruit"]
d[ d$recruit %in% seeds, "recruit" ] <- -1
d[ d$recruiter %in% seeds, "recruiter" ] <- -1
d <- d[ d$recruit != -1, ]

rds.df <- as.rds.data.frame(d, id = "recruit", network.size = "degree", population.size = 200000, max.coupons = 3, recruiter.id = "recruiter")
cumulative.estimate(rds.df, outcome.variable = "blk")

RDS.I.estimates(rds.df, outcome.variable = "testatus")
RDS.II.estimates(rds.df, outcome.variable = "testatus")

table(d.full$testatus)

RDS.I.estimates(rds.df, outcome.variable = "blk")
RDS.II.estimates(rds.df, outcome.variable = "blk")
reingold.tilford.plot(rds.df)