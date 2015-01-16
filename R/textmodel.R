setClass("Textmodel")

setClass("WordscoresModel", contains = "Textmodel")



setGeneric("train", function(model, ...){
    standardGeneric("train")
})


setMethod("train", signature(model="WordscoresModel"), function(model, ...){
    print("training a wordscores model")
})
