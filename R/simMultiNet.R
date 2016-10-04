#' Simulate multiple-edges network
#'
#' @param n Number of nodes
#' @param e Number of edges
#' @param directed Boolean, only directed currently supported
#' @param loops Boolean, whether to allow self-ties
#'
#' @return network object with multiple = TRUE
#' @export
#'
#' @importFrom network network.initialize
#' @importFrom network add.edges
#'
#' @examples
#' simMultiNet(5, 15)
simMultiNet = function(n, e, directed = TRUE, loops = FALSE) {

  net = network::network.initialize(n, directed = directed, loops = loops, multi = TRUE)

  edges = replicate(e, sample(seq_len(n), 2, replace = loops), simplify = FALSE)
  if(!directed) edges = lapply(edges, sort)
  edges = do.call(rbind, edges)

  network::add.edges(net, head = edges[, 1], tail = edges[, 2])

  return(net)

}
