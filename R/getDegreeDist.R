#' Title Get degree distributions
#'
#' @param graphs list of networks
#'
#' @return data.frame with degree, times observed in each graph, and graph number
#' @export
#' @importFrom sna degree
#' @importFrom gtools smartbind
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom magrittr %>%
#'
#' @examples
#' getDegreeDist(replicate(5, makeNetwork(10, .1)))
getDegreeDist = function(graphs) {
  # For a list of undirected networks, calculates degree distributions

  if(is.network(graphs))  { # Rather than a list of graphs
    deg = degree(graphs, gmode = "graph") %>%
      table() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      structure("names" = c('degree', 'count'))
    deg$degree = as.integer(deg$degree)
    deg = rbind(
      data.frame(degree = setdiff(1:max(deg$degree), unique(deg$degree)),
                 count = 0),
      deg) %>%
      arrange(degree)
    return(deg)
  }

  deg = degree(graphs, g = 1:length(graphs), gmode = 'graph') %>%
    split(., 1:ncol(.)) %>%
    lapply(table) %>%
    do.call(gtools::smartbind, .)  %>%
    tidyr::gather(degree, count) %>%
    # Need number of sims to join with correct number of 0s for degrees that never occur
    dplyr::mutate(.,
                  num = rep(1:length(graphs), length(unique(.$degree))),
                  degree = as.integer(degree)) %>%
    dplyr::arrange(degree)

  # Where smartbind fills with NA, there were no nodes of that degree, so replace with zero:
  deg[is.na(deg)] = 0
  deg
}
