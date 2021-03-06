% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/dataDocs.R
\docType{data}
\name{uk2010immig}
\alias{uk2010immig}
\title{Immigration-related sections of 2010 UK party manifestos}
\format{A named character vector of plain ASCII texts}
\description{
Extracts from the election manifestos of 9 UK political parties from 2010, related
to immigration or asylum-seekers.
}
\examples{
data(uk2010immig)
uk2010immigCorpus <- corpus(uk2010immig, docvars=list(party=names(uk2010immig)))
language(uk2010immigCorpus) <- "english"
encoding(uk2010immigCorpus) <- "UTF-8"
summary(uk2010immigCorpus)
}

