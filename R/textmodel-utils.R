#' Transform a list of ordinal target labels to a numeric vector
#' 
#' Returns a numeric vector for use as target variable for a textmodel that 
#' requires numeric response variables. This is appropriate for ordinal or
#' two-class classification, but not when the targets are non-transitive
#' 
#' @param categories A character vector of ordinal target values
#' @param values An optional list of numbers to map the ordinal values on to
#' @return A numeric vector of the same length as categories
#' @export
#' @author Paul Nulty
#' @examples
#' cats <- docvars(inaugCorpus, 'Year')
#' 
transformOrdinal <- function(categories, values=NULL){
    catFactor <- as.factor(categories)
    if(is.null(values)){
        return(as.numeric(catFactor))
    }
    if(length(levels(catFactor))!=length(values)){
        stop(sprintf('Number of values (%d) does not equal number of categories (%d)',length(values), length(levels(catFactor)) ))
    }
    
    tmp <- plyr::mapvalues(categories, levels(catFactor), values)
    return(as.numeric(tmp))
}

#' Perform cross-validation
#' 
#' Returns a numeric vector for use as target variable for a textmodel
#' 
#' @param dtm A dfm containing documents and features for training and testing.
#' @param values The 'true' target values for each document
#' @param model The model type to use for training and predictions
#' @param k The number of 'folds' to split the data into. Default is leave-one-out.
#' @export
#' @author Paul Nulty
crossVal <- function(dtm, values, k=nrow(dtm), model='wordscores'){
    foldSize <-  floor(nrow(dtm)/k)
    foldStart <- 1
    foldEnd <- 0
    heldoutPredictions <- data.frame(heldoutPredictions = c())
    while(foldStart <= nrow(dtm)){
        foldEnd <- foldEnd+(foldSize)
        if (foldEnd > nrow(dtm)){
            foldEnd <-  nrow(dtm)
        }
        thisTrain <- as.dfm(dtm[-(foldStart:foldEnd), , drop=FALSE])
        trainRefs <- values[-(foldStart:foldEnd)]
        thisMod <-  textmodel(thisTrain , y=trainRefs, model=c("wordscores"))
        print(sprintf("This holdout is %d to %d", foldStart, foldEnd))
        thisHoldout <- as.dfm(dtm[foldStart:foldEnd, , drop=FALSE])
        testRefs <- values[foldStart:foldEnd]
        print(foldStart)
        print(foldEnd)
        thisModel <- textmodel(thisTrain, trainRefs)
        thisResult <- predict(thisModel, thisHoldout, rescaling = "lbg")
        heldoutPredictions <- rbind(heldoutPredictions, thisResult)
        foldStart <- foldStart+foldSize
        
    }
    return(heldoutPredictions)
}