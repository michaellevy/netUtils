#' Title strong connectedness
#'
#' @param net network (can be sociomatrix)
#' @details sna::connectedness is based on weak reachability; that is, dyads are connected even if it takes going against the direction of links. This requires a link *from* i *to* j to consider i-j connected.
#'
#' @return fraction of dyads that are connected on directed paths
#' @export
#' @importFrom sna geodist
#'
#' @examples
#' n <- makeNetwork(10, .1, directed = TRUE)
#' strongConnectedness(n)
strongConnectedness = function(net) {
  reach = sna::reachability(net)
  return(mean(reach[upper.tri(reach) | lower.tri(reach)] > 0))
}
