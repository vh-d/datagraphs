
#' Union multiple datagraphs into a single datagraph object
#' @importFrom igraph union
#' @param ... args passed to union_list_of_graphs
#'
#' @export
#' @rdname union.datagraph
union.datagraph <- function(...) {
  union_list_of_graphs(list(...))
}

#' @param x list of datagraph objects
#'
#' @export
#' @rdname union.datagraph
union_list_of_graphs <- function(x) {
  newgraph <- datagraph()
  for (i in x) {
    coi <- copy_of(i)
    list2env(as.list.environment(coi, sorted = FALSE), newgraph)
    list2env(as.list.environment(coi[[".edges"]], sorted = FALSE), newgraph[[".edges"]])
  }
  return(newgraph)
}
