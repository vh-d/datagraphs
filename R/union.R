
#' @importFrom igraph union
#' @export
union.datagraph <- function(...) {
  union_list_of_graphs(list(...))
}

#' @export
union_list_of_graphs <- function(x) {
  newgraph <- datagraph()
  for (i in x) {
    coi <- copy_of(i)
    list2env(as.list.environment(coi, sorted = FALSE), newgraph)
    list2env(as.list.environment(coi[[".edges"]], sorted = FALSE), newgraph[[".edges"]])
  }
  return(newgraph)
}
