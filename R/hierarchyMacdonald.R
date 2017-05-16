#' Title Hierarchy (MacDonald)
#'
#' @param net network, probably directed
#'
#' @return hierarchy in [0, 1]
#' @export
#'
#' @references MacDonald 1983 *Trees and Networks in Biological Models*
#' @details Note that this hierarchy is quite different from Krackhardt's or reciprocal, both of which are available in sna::hierarchy().
#'
#' @examples
#' hierarchyMacdonald(makeNetwork(5, .1, dir = TRUE))
hierarchyMacdonald = function(net) {

  if (!is.directed(net))
    warning(deparse(substitute(net)), " is undirected. You sure you want to calculate MacDonald's hierarchy on it?\nProceeding using total degree...")

  odeg = sna::degree(net, cmode = "outdegree")
  N = network::network.size(net)

  h = 12 * sum((odeg - mean(odeg))^2) / (N * (N-1) * (N+1))
  return(h)
}
