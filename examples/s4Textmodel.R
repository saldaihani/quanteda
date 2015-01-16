library(quantedaData)
data(sotuCorp)
sotuDfm <-  trimdfm(dfm(sotuCorp), minCount=4, minDoc=4)

mod <- new(Class="WordscoresModel")
train(mod)
