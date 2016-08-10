#' Title Make a network ob with (stochastic) density
#'
#' @param nSize Number of nodes
#' @param nDensity Probability of each edge
#'
#' @return network object
#' @export
#' @importFrom network network
#'
#' @examples
#' makeNetwork(10, .05)
#' makeNetwork(10, .05, TRUE)
makeNetwork = function (nSize, nDensity, directed = FALSE) {

  if(directed) {
    m = matrix(rbinom(nSize^2, 1, nDensity), nrow = nSize)
  } else {
    m = matrix(rep(0, nSize^2), nrow = nSize)
    m[upper.tri(m)] = rbinom((nSize^2 - nSize)/2, 1, nDensity)
  }
  network(m, directed = directed)
}
