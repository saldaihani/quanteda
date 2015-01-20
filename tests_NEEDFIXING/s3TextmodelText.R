# Testing script for modifications made to s3 textmodel file. 
# PN 19 Jan 15

library(quantedaData)
library(ggplot2)
library(dplyr)
library(tidyr)
data(sotuCorp)

sdfm <- dfm(sotuCorp)



sdfm <- trimdfm(sdfm, minCount=3, minDoc=3)
wfmod <- textmodel(sdfm, model=('wordfish'),dir=c(-5,5))


#sdfm <- weight(sdfm, smooth=0.1)


numTargets <- transformOrdinal(docvars(sotuCorp, 'party'),c(-5,5))




pr <- crossVal(sdfm, numTargets, k=30)

thetas <- wfmod$theta

plotdf <- data.frame(docScores = pr$textscore_lbg,
                     wfishScores = thetas,
                     year=docvars(sotuCorp, 'year'))
plotdf <- gather(plotdf, 'method','score', -year)
clrs <- plyr::mapvalues(docvars(sotuCorp,'party'), c('rep','dem'),  c('red','blue'))

qplot(score, paste(year), data=plotdf, colour=method) +
    theme(axis.text.y = element_text(colour = clrs))



