#' Detect collocations from text
#'
#' Detects collocations (currently, bigrams) from texts or a corpus, returning a 
#' data.frame of collocations and their scores, sorted by the likelihood
#' ratio \eqn{G^2} and Pearson's \eqn{\chi^2}.
#' @param x a text, a character vector of texts, or a corpus
#' @param method association measure for detecting collocations.  \code{all} returns 
#' all available measures, \code{lr} returns the likelihood ratio statistic \eqn{G^2},
#' and \code{chi2} returns Pearson's \eqn{\chi^2} statistic.
#' @param n length of the collocation.  Only bigrams (\code{n=2}) implemented so far.
#' @param top the number of collocations to return, sorted in descending order of the 
#' requested statistic, or \eqn{G^2} if none is specified.
#' @param ... additional parameters
#' @return A data.frame of collocations, their frequencies, and the computed
#' association measure.
#' @export 
#' @author Kenneth Benoit
#' @examples
#' collocations(inaugTexts, top=10)
#' collocations(inaugCorpus, top=10, method="chi2")
collocations <- function(x, ...) {
    UseMethod("collocations")
}
    
#' @rdname collocations
#' @export    
collocations.character <- function(x, method=c("all", "lr", "chi2"), n=2, top=NULL, ...) {
    method <- match.arg(method)
    if (n != 2) stop("Only bigrams (n=2) implemented so far.")

    # to prevent warning messages during CHECK
    #w1 <- w2 <- count <- w1w2n <- w1w2Exp <- w1notw2Exp <- notw1w2 <- notw1w2Exp <- NULL
    #notw1notw2 <- notw1notw2Exp <- NULL
    
    text <- clean(x, ...)
    t <- unlist(tokenize(text), use.names=FALSE)
    
    # create a data.table of all adjacent bigrams
    wordpairs <- data.table(w1 = t[1:(length(t)-1)], 
                            w2 = t[2:length(t)], 
                            count = 1)
    
    # set the data.table sort key
    setkey(wordpairs, w1, w2)
    
    # tabulate (count) w1 w2 pairs
    wordpairsTable <- wordpairs[, j=sum(count), by="w1,w2"]
    setnames(wordpairsTable, "V1", "w1w2n")
    
    # tabulate all word marginal counts
    w1Table <- wordpairs[, sum(count), by=w1]
    setnames(w1Table, "V1", "w1n")
    setkey(w1Table, w1)
    
    setkey(wordpairsTable, w1)
    suppressWarnings(allTable <- wordpairsTable[w1Table])
    # otherwise gives an encoding warning
    
    # tabulate all w2 counts
    w2Table <- wordpairs[, sum(count), by=w2]
    setnames(w2Table, "V1", "w2n")
    setkey(w2Table, w2)
    
    setkey(allTable, w2)
    suppressWarnings(allTable2 <- allTable[w2Table])
    # otherwise gives an encoding warning
    
    setkey(allTable2, w1, w2)
    
    N <- sum(allTable2$w1w2n)  # total number of collocations (table N for all tables)
    
    # fill in cells of 2x2 tables
    allTable2$w1notw2 <- allTable2$w1n - allTable2$w1w2
    allTable2$notw1w2 <- allTable2$w2n - allTable2$w1w2
    allTable2$notw1notw2 <- N - (allTable2$w1w2 + allTable2$w1notw2 + allTable2$notw1w2)
    
    # calculate expected values
    allTable2$w1w2Exp <- allTable2$w1n * allTable2$w2n / N
    allTable2$w1notw2Exp <- allTable2$w1n * (N - allTable2$w2n) / N
    allTable2$notw1w2Exp <- allTable2$w2n * (N - allTable2$w1n) / N
    allTable2$notw1notw2Exp <- (N - allTable2$w2n) * (N - allTable2$w1n) / N
    
    # vectorized lr stat
    epsilon <- .000000001  # to offset zero cell counts
    if (method=="all" | method=="lr") {
        allTable2$lrratio <- 2 *  ((allTable2$w1w2n * log(allTable2$w1w2n / allTable2$w1w2Exp + epsilon)) +
                                       (allTable2$w1notw2 * log(allTable2$w1notw2 / allTable2$w1notw2Exp + epsilon)) +
                                       (allTable2$notw1w2 * log(allTable2$notw1w2 / allTable2$notw1w2Exp + epsilon)) +
                                       (allTable2$notw1notw2 * log(allTable2$notw1notw2 / allTable2$notw1notw2Exp + epsilon)))
    }
    if (method=="all" | method=="chi2") {
        allTable2$chi2 <- (allTable2$w1w2n - allTable2$w1w2Exp)^2 / allTable2$w1w2Exp +
        (allTable2$w1notw2 - allTable2$w1notw2Exp)^2 / allTable2$w1notw2Exp +
        (allTable2$notw1w2 - allTable2$notw1w2Exp)^2 / allTable2$notw1w2Exp +
        (allTable2$notw1notw2 - allTable2$notw1notw2Exp)^2 / allTable2$notw1notw2Exp
    }    
    if (method=="chi2") {
        allTable2 <- allTable2[order(-chi2)]
        df <- data.frame(collocation=paste(allTable2$w1, allTable2$w2),
                         count=allTable2$w1w2n,
                         X2=allTable2$chi2)
    } else {
        allTable2 <- allTable2[order(-lrratio)]
        df <- data.frame(collocation=paste(allTable2$w1, allTable2$w2),
                         count=allTable2$w1w2n,
                         G2=allTable2$lrratio) 
        if (method=="all") df$X2 <- allTable2$chi2
    }
    
    df[1:ifelse(is.null(top), N, top), ]
}

#' @rdname collocations
#' @export
collocations.corpus <- function(x, method=c("all", "lr", "chi2"), n=2, top=NULL, ...) {
    collocations(texts(x), method, n, top, ...)
}



