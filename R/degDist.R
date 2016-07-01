#' Degree distribution
#'
#' @param net network object
#' @param form one of \code{c("p", "count")}. "p" (default) returns the proportion of nodes with each degree. Anything else returns the count, and if \code{df == TRUE} this value will be the name of the variable in the data.frame.
#' @param df if \code{TRUE} returns a data.frame; if \code{FALSE} (default), returns a table.
#'
#'
#' @return By default, a table of degree counts from 0 to max degree. If \code{df == TRUE}, a data.frame with a column for degree and a column for \code{form}.
#'
#' @export
#'
#' @examples
#' n <- network(rgraph(10, tprob = .1))
#' degDist(n)
#' degDist(n, "count", df = TRUE)

degDist <- function(net, form = "p", df = FALSE) {

  gmode <- if (network::is.directed(net)) "digraph" else "graph"
  degs <- sna::degree(net, gmode = gmode)
  degs <- table(factor(degs, levels = 0:max(degs)))
  if (form == "p")
    degs <- degs / network::network.size(net)
  if (df)
    degs <- structure(as.data.frame(degs), names = c("degree", form))

  return(degs)
}

