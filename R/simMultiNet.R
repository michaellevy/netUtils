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

  heads = sample(seq_len(n), e, replace = TRUE)
  tails = sample(seq_len(n), e, replace = TRUE)

  # Either this assign tails with by sapplying for positions.
  # The bigger the network, the less likely loops are,
  # so I don't think this will loop too many times
  if(!loops)
    while(any(heads == tails)) {
      repl = which(heads == tails)
      tails[repl] = sample(seq_len(n), length(repl), replace = TRUE)
    }

  network::add.edges(net, head = heads, tail = tails)

  return(net)

}
