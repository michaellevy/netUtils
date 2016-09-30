#' Make a network ob with exact density or average degree
#'
#' @param n Number of nodes
#' @param density Probability of each edge
#' @param meanDegree Mean degree of nodes (total degree, if directed)
#' @param directed The gmode, as it were
#'
#' @return network object
#' @export
#' @importFrom network network
#'
#' @details Tries to make a network object with exactly the density or mean
#' degree specified; may be slightly off due to rounding, e.g. a 5-node network
#' with meanDegree = .1 implies one-quarter of an edge for the whole network, which
#' will get rounded down to produce an empty network.
#'
#' @examples
#' makeNetwork(10, .05)
#' makeNetwork(10, .05, directed = TRUE)
#' makeNetwork(100, meanDegree = .1)
#' makeNetwork(5, meanDegree = .1)  # Note rounding
makeNetwork = function (n, density = NULL, meanDegree = NULL, directed = FALSE) {

  if(class(density) == class(meanDegree))
    stop("You have to provide either density or meanDegree")

  m = matrix(rep(0L, n^2), nrow = n)
  eligible = if(directed) which(upper.tri(m) | lower.tri(m)) else which(upper.tri(m))

  if(!is.null(density)) {
    # trunc(.5 + x) is rounding because round() is stupid about .5's
    edgePositions = sample(eligible, trunc(.5 + length(eligible) * density))
  } else {
    edgePositions = sample(eligible, trunc(.5 + n * meanDegree / 2))
  }

  m[edgePositions] = 1
  network(m, directed = directed)
}
