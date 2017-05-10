#' Vertex attributes as a data.frame
#'
#' @param net the network
#'
#' @return data.frame with vertex.names as first column, vertex.attributes as rest
#' @export
#' @importFrom network list.vertex.attributes
#' @importFrom network get.vertex.attribute
#'
#' @examples
vAttrDF = function(net) {
  ats = network::list.vertex.attributes(net)
  ats = structure(data.frame(
    lapply(ats, function(vAt) network::get.vertex.attribute(net, vAt))
    ), names = ats)
  # Remove annoying "na" attribute if none are missing
  ats = ats[, -which(names(ats) == "na")]
  vn = colnames(ats) == "vertex.names"
  ats[, c(which(vn), which(!vn))]
}
