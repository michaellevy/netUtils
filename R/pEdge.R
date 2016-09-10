#' Title Calculate edge probabilities implied by an ERGM
#'
#' @param mod ERGM
#' @param net network, taken from ERGM by default
#' @param edgelist two-column edgelist, by default will create one with all possible edges present
#'
#' @return data.frame with edgelist and column of edge probabilities
#' @export
#' @importFrom network is.directed
#' @importFrom network network.size
#' @importFrom network get.network.attribute
#' @importFrom ergm summary.formula
#' @importFrom network get.edgeIDs
#' @importFrom network delete.edges
#' @importFrom network add.edge
#'
#' @examples
#' \dontrun{
#' set.seed(1111)
#' n = makeNetwork(25, .1, directed = TRUE)
#' m = ergm(n ~ edges + mutual)
#' probs = pEdge(m)
#' head(probs, n = 30)
#' }
#'
#' @details For any decent size network this will take a while because each edge has to be added to an removed from the network. So, it's probably worth compiling the function with `c_pedge = compiler::compile(pEdge)`.
pEdge = function(mod, net = mod$network, edgelist = NULL) {

  gmode = if(is.directed(net)) 'digraph' else 'graph'

  if(is.null(edgelist)) {  # make EL with all possible edges in the network
    edgelist = structure(expand.grid(replicate(2, 1:network.size(net), FALSE)),
                         names = c("node1", "node2"))

    if(gmode == 'graph')
      edgelist = edgelist[edgelist[, 1] <= edgelist[, 2], ]
    if(!get.network.attribute(net, "loops"))
      edgelist = edgelist[edgelist[, 1] != edgelist[, 2], ]

  } else { # Test its structure
    if(ncol(edgelist) != 2 | !all(sapply(edgelist, is.numeric)))
      stop('edgelist has to have two numeric columns, one for edge heads and one for tails')
    if(gmode == 'graph' &&
       length(unique(edgelist[, 1] < edgelist[, 2])) != 1)  # If not all node1 > node2 or vice-versa
      warning('Are you sure you are not running redundant edges? net is undirected but not all node1 > (or <) node2')
  }

  coefs = mod$coef
  modelFormula = mod$formula
  baselineStats = summary(modelFormula, basis = net)

  p =
    sapply(1:nrow(edgelist), function(i) {
      h = edgelist[i, 1]
      t = edgelist[i, 2]
      eid = get.edgeIDs(net, h, t)
      # If the edge exists (ie, length(eid) != 0), delete it, calc stats, and return baseline - new
      if(length(eid)) {
        delete.edges(net, eid)
        val = plogis(sum(coefs * (baselineStats - summary(modelFormula, basis = net))))
        add.edge(net, h, t)
      } else {
        add.edge(net, h, t)
        val = plogis(sum(coefs * (summary(modelFormula, basis = net) - baselineStats)))
        delete.edges(net, get.edgeIDs(net, h, t))
      }
      val
    })

  # If there are constraints in the ERGM, e.g. number of edges rather than a density term, the relative probabilities will be right, but absolute values will be off.
  # However, the average probability of an edge has to be density of the graph, so dividing the calculated mean and multiplying by the empirical density corrects this.
  odds = p / (1 - p)
  trueMeanOdds = network.density(net) / (1 - network.density(net))
  adjustedOdds = odds * trueMeanOdds / mean(odds)
  edgelist$p = adjustedOdds / (1 + adjustedOdds)

  return(edgelist)
}
