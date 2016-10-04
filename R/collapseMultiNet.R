#' Make a simple network with edge attribute from a multiplex network
#'
#' @param net Network
#'
#' @return Network with edge attribute "NumberTies"
#' @export
#' @importFrom networkDynamic get.edge.activity
#' @importFrom dplyr count
#' @importFrom network network
#' @importFrom network is.directed
#' @importFrom network list.vertex.attributes
#' @importFrom network set.vertex.attribute
#' @importFrom network get.vertex.attribute
#'
#' @examples
#' collapseMultiNet(simMultiNet(10, 1e3))
#' collapseMultiNet(simMultiNet(10, 1e3, directed = FALSE))
collapseMultiNet = function(net) {

  vAt = sapply(list.vertex.attributes(net), function(x) get.vertex.attribute(net, x),
               simplify = FALSE, USE.NAMES = TRUE)

  dir = is.directed(net)
  el = networkDynamic::get.edge.activity(net, as.spellList = TRUE)[, 3:4]
  el = dplyr::count(el, tail, head)
  n = network::network(el, directed = dir,
                       ignore.eval = FALSE, names.eval = 'NumberTies')
  for(at in names(vAt))
    set.vertex.attribute(n, at, vAt[[at]])
  return(n)

}
