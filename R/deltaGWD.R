#' Title delta GWD
#'
#' @param k Node degree
#' @param theta_s Decay parameter value
#'
#' @return Change to GWD statistic for adding a half-edge to a node of degree-k
#' @export
#'
#' @examples
#' deltaGWD(1:10, 1.5)
deltaGWD = function(k, theta_s) {
  (1 - exp(-theta_s))^k
}
