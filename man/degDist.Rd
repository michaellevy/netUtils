% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/degDist.R
\name{degDist}
\alias{degDist}
\title{Degree distribution}
\usage{
degDist(net, form = "p", df = FALSE)
}
\arguments{
\item{net}{network}

\item{form}{one of \code{c("p", "count")}. "p" (default) returns the proportion of nodes with each degree. Anything else returns the count, and if \code{df == TRUE} this value will be the name of the variable in the data.frame.}

\item{df}{if \code{TRUE} returns a data.frame; if \code{FALSE} (default), returns a table.}
}
\value{
By default, a table of degree counts from 0 to max degree. If \code{df == TRUE}, a data.frame with a column for degree and a column for \code{form}.
}
\description{
Degree distribution
}
\examples{
n <- makeNetwork(10, .1)
degDist(n)
degDist(n, "count", df = TRUE)
}
