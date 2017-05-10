#' Harmonic mean path length
#'
#' @param net network object. if `net` is directed, will be made undirected by the weak rule (if i->j or j->i then i-j).
#' @param makeUndirected Should directed network be made undirected by weak rule? Default is FALSE
#'
#' @return Harmonic mean of path lengths between dyads.
#' @references Newman MEJ, 2003. The structure and function of complex networks. SIAM Review.
#' @export
#' @importFrom network is.directed
#' @importFrom sna symmetrize
#' @importFrom sna geodist
#'
#' @examples
#' n <- makeNetwork(10, .1)
#' HMPL(n)
#' n <- makeNetwork(10, .1, directed = TRUE)
#' HMPL(n)
HMPL <- function(net, makeUndirected = FALSE) {

  if (makeUndirected) {
    if (!is.directed(net)) {
      stop("You said to make ", deparse(substitute(net)), " undirected, but it already is.")
    } else {
      net <- symmetrize(net[,], rule = "weak")
    }
  }

  d = sna::geodist(net)[["gdist"]]
  d = d[upper.tri(d) | lower.tri(d)]
  return(mean(d^-1)^-1)
}
