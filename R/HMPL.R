#' Harmonic mean path length
#'
#' @param net network object. if `net` is directed, will be made undirected by the weak rule.
#'
#' @return Harmonic mean path length between pairs of nodes.
#' @references Newman MEJ, 2003. The structure and function of complex networks. SIAM Review.
#' @export
#' @importFrom network is.directed
#' @importFrom sna symmetrize
#' @importFrom sna geodist
#'
#' @examples
#' n <- network(rgraph(10, tprob = .1))
#' HMPL(n)

HMPL <- function(net) {

  if (is.directed(net)) {
    warning(deparse(substitute(net)), " is directed. HMPL only work for undirected networks. Converting... ")
    net <- symmetrize(net[,], rule = "weak")
  }

  d = geodist(net)[["gdist"]]
  return(mean(d[upper.tri(d)]^-1)^-1)

}
