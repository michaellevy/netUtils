
#' Clustering coefficient
#'
#' @param net network object
#'
#' @return clustering coefficient, \deqn{\dfrac{3 \times triangles}{two-paths}}
#' @export
#' @importFrom ergm summary.formula
#'
#' @examples
#' n <- network(rgraph(10, tprob = .3))
#' clusteringCoef(n)

clusteringCoef = function(net) {
  unname(3 * summary(net ~ triangles) / summary(net ~ twopath))
}
