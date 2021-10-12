
#' @export
copy_of.datagraph_vertex <- function(x) {
  nv <- datagraph_vertex()
  nv[["id"]] <- x[["id"]]
  if (!is.null(x[["data"]])) list2env(as.list.environment(x[["data"]], sorted = FALSE), nv[["data"]])
  return(nv)
}

#' @export
copy_of.datagraph <- function(x) {
  ng <- datagraph()
  newel <- ng[[".edges"]]
  for (i in as.list.environment(x, sorted = FALSE)) {
    nv <- copy_of.datagraph_vertex(i)
    assign(nv[["id"]], nv, envir = ng)
  }
  for (j in as.list.environment(x[[".edges"]], sorted = FALSE)) {
    fromid <- j[["from"]][["id"]]
    toid   <- j[["to"]][["id"]]
    add_edge.character(fromid, toid, graph = ng, data = j[["data"]])
  }
  return(ng)
}
