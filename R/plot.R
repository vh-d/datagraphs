#' @export
plot.datagraph <- function(x, ...) {
  igraph::plot.igraph(as.igraph(x), ...)
}
