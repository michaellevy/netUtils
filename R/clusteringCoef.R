#' Clustering coefficient
#'
#' @param net network object
#'
#' @return clustering coefficient, \deqn{\frac{3 \times triangles}{two-paths}}
#' @export
#' @importFrom ergm summary.formula
#' @importFrom network network
#'
#' @examples
#' n <- makeNetwork(10, .3)
#' clusteringCoef(n)

clusteringCoef = function(net) {

  if (is.directed(net)) {
    warning("Converting the network to undirected")
    net = network(symmetrize(net[,]), directed = FALSE)
  }
  unname(3 * summary(net ~ triangles) / summary(net ~ twopath))
}
