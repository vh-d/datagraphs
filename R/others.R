
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
all.equal.datagraph_vertex <- function(current, target) {
  all.equal.environment(current, target)
}

#' @export
check.datagraph <- function(x) {
  el <- x[[".edges"]]
  if (!is.datagraph_edgelist(el)) return("Graph corrupted. Not an edgelist")
  elids <- names(el)

  l <- as.list(x, sorted = FALSE)
  for (i in l) {
    if (!is.datagraph_vertex(i)) return(FALSE)
    if (!is.datagraph_edgelist(i[["from"]])) return(FALSE)
    if (!is.datagraph_edgelist(i[["to"]])) return(FALSE)

    for (j in as.list.environment(i[["from"]], sorted = FALSE)) {
      if (!is.datagraph_edge(j)) return("Graph corrupted. Not a datagraph_edge.")
      if (!identical(j[["to"]], i)) return("Graph corrupted.  Edge end point does not match.")
      if (!(j[["id"]] %in% elids)) return("Graph corrupted. Edge is missing in the edgelist.")
    }

    for (k in as.list.environment(i[["to"]], sorted = FALSE)) {
      if (!is.datagraph_edge(k)) return("Graph corrupted. Not a datagraph_edge.")
      if (!identical(k[["from"]], i)) return("Graph corrupted. Edge starting point does not match.")
      if (!(k[["id"]] %in% elids)) return("Graph corrupted. Edge is missing in the edgelist.")
    }
  }
  return(TRUE)
}


#' @export
`[.datagraph` <- function(x, i, j) {
  iexp <- if (missing(i)) quote(TRUE) else substitute(i)
  jexp <- if (missing(j)) quote(data.table(id = id, from = list(from), to = list(to))) else substitute(j)

  i <- tryCatch(i, error = function(e) NULL )
  if (!is.character(i)) {
    ires <- sapply(as.list.environment(x, sorted = FALSE), eval, expr = iexp)
    ids <- names(which(ires))
  } else {
    ids <- i
  }

  rbindlist(lapply(mget(x = ids, envir = x), eval, expr = jexp))
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
  subgraph <- list2env(mget(subset, envir = x, mode = "environment"))
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
