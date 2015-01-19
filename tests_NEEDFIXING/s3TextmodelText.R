# Testing script for modifications made to s3 textmodel file. 
# PN 19 Jan 15

library(quantedaData)
library(ggplot2)
library(dplyr)
data(sotuCorp)

sdfm <- dfm(sotuCorp)
#sdfm <- weight(sdfm, smooth=0.1)

numTargets <- transformOrdinal(docvars(sotuCorp, 'party'),c(5,-5))

refScores <- rep(NA,30)


# reagan and bush2
refScores[2] <- 5  
refScores[18] <- 5 

# clinton and obama
refScores[10] <- -5 
refScores[26] <- -5 

wmod <- textmodel(sdfm , y=refScores, model=c("wordscores"))
results <- predict(wmod, sdfm, rescaling = "mv")

plotdf <- data.frame(docScores = results$textscore_lbg,
                     party=docvars(sotuCorp, 'party'),
                     year=docvars(sotuCorp, 'year'))

ploarrange(plotdf, year)
