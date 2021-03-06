#' Calculate edge probabilities implied by an ERGM
#'
#' @param mod ERGM
#' @param net network, taken from ERGM by default
#' @param edgelist two-column edgelist, by default will create one with all possible edges present
#' @param returnEdgelist want the two columns with the edgelist back?
#' @param returnObserved want the (dichotomous) observed edge values back?
#' @details if !returnEdgelist & !returnObserved, you get a vector back. Otherwise, a data.frame
#'
#' @return data.frame with edgelist and column of edge probabilities
#' @export
#' @importFrom network is.directed
#' @importFrom network network.size
#' @importFrom network get.network.attribute
#' @importFrom ergm summary.formula
#' @importFrom ergm ergm
#' @importFrom network get.edgeIDs
#' @importFrom network delete.edges
#' @importFrom network add.edge
#' @importFrom network network.density
#' @importFrom stats plogis
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
pEdge = function(mod, net = mod$network, edgelist = NULL, returnEdgelist = TRUE, returnObserved = FALSE) {

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


  if(any(is.infinite(coefs)))
    warning("Infinite coefficients detected. They will be ignored for edges where their change statistic is zero.")

  p =
    do.call(rbind,
            lapply(1:nrow(edgelist), function(i) {
              h = edgelist[i, 1]
              t = edgelist[i, 2]
              eid = get.edgeIDs(net, h, t)
              # If the edge exists (ie, length(eid) != 0), delete it, calc stats, and return baseline - new
              if(length(eid)) {
                delete.edges(net, eid)
                val = plogis(sum(coefs * (baselineStats - summary(modelFormula, basis = net)), na.rm = TRUE))
                add.edge(net, h, t)
              } else {
                add.edge(net, h, t)
                val = plogis(sum(coefs * (summary(modelFormula, basis = net) - baselineStats), na.rm = TRUE))
                delete.edges(net, get.edgeIDs(net, h, t))
              }
              data.frame(node1 = h, node2 = t, actual = if(length(eid)) 1 else 0, p = val)
            })
    )

  # If there are constraints in the ERGM, e.g. number of edges rather than a density term, the relative probabilities will be right, but absolute values will be off.
  # However, the average probability of an edge has to be density of the graph, so dividing the calculated mean and multiplying by the empirical density corrects this.
  # Note that this doesn't get applied to ERGMs w/o a density term, so they will have a mean prob of .5 (I think)
  if(!(is.null(mod$constraints[[2]]) || mod$constraints[[2]] == ".")) {   # Test for any constraints
    odds = p$p / (1 - p$p)
    trueMeanOdds = network.density(net) / (1 - network.density(net))
    adjustedOdds = odds * trueMeanOdds / mean(odds)
    p$p = adjustedOdds / (1 + adjustedOdds)
  }

  if(!returnObserved)
    p = p[, -which(names(p) == "actual")]

  if(!returnEdgelist)
    p = p[, -grep("node", names(p))]

  return(p)
}
