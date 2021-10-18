
#' @export
all.equal.datagraph <- function(current, target) {
  if (!setequal(ls(current, sorted = FALSE), ls(target, sorted = FALSE))) return("Different set of vertices")

  res2 <- mapply(all.equal, as.list(current), as.list(target))
  if (!(is.logical(res2))) {
    if (!length(res2)) return(TRUE)
    if (is.list(res2)) {
      return(names(which(sapply(res2, is.character), TRUE)))
    }
    return(res2)
  }

  return(TRUE)
}


#' @export
all.equal.datagraph_vertex <- function(current, target, ...) {
  if (!identical(current[["id"]], target[["id"]])) return("Different ids")
  if (length(setdiff(neighbors_in(current),  neighbors_in(target)))) return("Different set of incoming links")
  if (length(setdiff(neighbors_out(current), neighbors_out(target)))) return("Different set of outgoing links")
  if (!(setequal(names(current[["data"]]), names(target[["data"]])))) return("Different set of attributes")
  if (length(current[["data"]])) {
    return(all.equal(current[["data"]], target[["data"]]))
  }
  return(TRUE)
}

#' @export
all.equal.datagraph_edgelist <- function(current, target, ...) {
  all.equal.environment(current, target, all.names = TRUE, evaluate = FALSE)
}

#' @export
check.datagraph <- function(x) {
  el <- x[[".edges"]]
  if (!is.datagraph_edgelist(el)) return("Graph corrupted. Not an edgelist")
  vids <- V(x)
  elids <- names(el)
  elcheck <- rep(FALSE, length(el))
  names(elcheck) <- elids

  l <- as.list(x, sorted = FALSE)
  for (i in l) {
    if (!is.datagraph_vertex(i)) return("Graph corrupted. Not a vertex.")
    if (!is.datagraph_edgelist(i[["from"]])) return("Graph corrupted. Not an edgelist.")
    if (!is.datagraph_edgelist(i[["to"]])) return("Graph corrupted. Not an edgelist.")

    if (!all(names(i[["from"]]) %in% vids)) return("Graph corrupted. Edges outside of the graph.")
    for (j in as.list.environment(i[["from"]], sorted = FALSE)) {
      if (!is.datagraph_edge(j)) return("Graph corrupted. Not a datagraph_edge.")
      if (!identical(j[["to"]], i)) return("Graph corrupted.  Edge end point does not match.")
      if (!(j[["id"]] %in% elids)) return("Graph corrupted. Edge is missing in the edgelist.")
      if (!(j[["from"]][["id"]] %in% vids)) return("Graph corrupted. Edge is missing in the edgelist.")
      elcheck[j[["id"]]] <- TRUE
    }

    if (!all(names(i[["to"]]) %in% vids)) return("Graph corrupted. Edges outside of the graph.")
    for (k in as.list.environment(i[["to"]], sorted = FALSE)) {
      if (!is.datagraph_edge(k)) return("Graph corrupted. Not a datagraph_edge.")
      if (!identical(k[["from"]], i)) return("Graph corrupted. Edge starting point does not match.")
      if (!(k[["id"]] %in% elids)) return("Graph corrupted. Edge is missing in the edgelist.")
      elcheck[k[["id"]]] <- TRUE
    }
  }

  for (eid in elids) {
    i <- el[[eid]]
    if (!is.datagraph_edge(i)) return("Graph corrupted. Not an edge.")
    vfrom <- i[["from"]]
    vto   <- i[["to"]]
    if (!is.datagraph_vertex(vfrom)) return("Graph corrupted. Not a vertex.")
    if (!is.datagraph_vertex(vto)) return("Graph corrupted. Not a vertex.")
    fromid <- vfrom[["id"]]
    toid   <- vto[["id"]]
    eid2 <- sprintf("%s->%s", fromid, toid)
    if (!identical(eid, eid2)) return('Graph corrupted. Edge name does not correspond to data')
    if (!identical(eid, i[["id"]])) return('Graph corrupted. Edge name does not correspond to data')
  }
  if (!all(elcheck)) return("Graph corrupted. Extra edges in the edgelist.")
  return(TRUE)
}

eval_in_data <- function(x, expr, ...) {
  tryCatch(eval(expr = expr, envir = x[["data"]], ...), error = function(e) list())
}

#' @export
`[.datagraph` <- function(x, i, j) {
  iexp <- if (missing(i)) quote(TRUE) else substitute(i)
  jexp <- if (missing(j)) quote(data.table(id = id, from = list(names(from)), to = list(names(to)))) else substitute(j)

  i <- tryCatch(i, error = function(e) NULL )
  if (!is.character(i)) {
    ires <- sapply(as.list.environment(x, sorted = FALSE), eval_in_data, expr = iexp)
    ids <- names(which(ires))
  } else {
    ids <- i
  }

  res <- lapply(mget(x = ids, envir = x), eval_in_data, expr = jexp)
  if (is.list(res[[1]])) rbindlist(res) else unlist(res, recursive = FALSE)
}


#' @export
detect_cycles.datagraph <- function(graph) {
  newgraph <- list2env(as.list(graph))
  len <- length(newgraph)
  continue <- TRUE
  while (continue) {
    for (i in ls(newgraph, sorted = FALSE)) {
      n <- newgraph[[i]]
      if (!(length(n[["from"]]) && length(n[["to"]]))) {
        remove(list = i, envir = newgraph)
      }
      if (len == length(newgraph)) continue <- FALSE else len <- length(newgraph)
    }
  }

  class(newgraph) <- c("datagraph_subgraph", "datagraph")

  return(newgraph)
}


#' @export
subset.datagraph <- function(x, subset) {
  subset <- intersect(V(x), subset)
  gl <- mget(subset, envir = x, mode = "environment")
  subgraph <- list2env(gl)
  el <- edgelist()
  subgraph[[".edges"]] <- el

  for (i in gl) {
    id <- i[["id"]]
    for (j in as.list.environment(i[["from"]])) {
      eid <- j[["id"]]
      fromid <- j[["from"]][["id"]]
      if (fromid %in% subset) {
        el[[eid]] <- j
      } else {
        remove_neighbor_in(i, j)
        remove_neighbor_out(j, i)
      }
    }

    for (k in as.list.environment(i[["to"]])) {
      eid <- k[["id"]]
      toid <- k[["to"]][["id"]]
      if (toid %in% subset) {
        el[[eid]] <- k
      } else {
        remove_neighbor_in(k, i)
        remove_neighbor_out(i, k)
      }
    }

  }
  class(subgraph) <- c("datagraph_subgraph", "datagraph")

  return(subgraph)
}


#' @export
collapse_vertices.datagraph <- function(x, vertices) {
  vs <- mget(vertices, envir = x, inherits = FALSE, ifnotfound = list(NULL))
  vertices <- vertices[!is.null(vs)]
  vs <- vs[!is.null(vs)]
  if (length(vs) < 2L) return(invisible(x))

  edges <- x[[".edges"]]
  v <- vs[[1]]
  vid <- v[["id"]]

  for (i in vs[-1]) {
    for (j in as.list.environment(i[["to"]], sorted = FALSE)) {
      relink_edge(j, graph = x, from = v)
    }

    for (j in as.list.environment(i[["from"]], sorted = FALSE)) {
      relink_edge(j, graph = x, to = v)
    }

    remove(list = i[["id"]], envir = x)
  }

  return(invisible(x))
}
