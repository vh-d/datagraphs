#' @export
datagraph_edge <- function(parent = emptyenv()) {
  e <- structure(new.env(parent = parent), class = "datagraph_edge")
  return(e)
}

#' @export
is.datagraph_edge <- function(x) {
  "datagraph_edge" %in% class(x)
}


#' @export
edgelist <- function(parent = emptyenv()) {
  el <- structure(new.env(parent = parent), class = "datagraph_edgelist")
  return(el)
}

#' @export
is.datagraph_edgelist <- function(x) {
  "datagraph_edgelist" %in% class(x)
}


#' @export
datagraph_vertex <- function(parent = parent.frame()) {
  v <- new.env(parent = parent)
  v[["from"]] <- edgelist(parent = v)
  v[["to"]]   <- edgelist(parent = v)
  v[["data"]] <- new.env(parent = v)
  class(v) <- c("datagraph_vertex")
  return(v)
}

#' @export
is.datagraph_vertex <- function(x) {
  "datagraph_vertex" %in% class(x)
}


#' @export
datagraph <- function() {
  graph <- new.env(parent = parent.frame())
  graph[[".edges"]] <- edgelist(parent = graph)
  class(graph) <- c("datagraph")
  return(graph)
}

#' @export
is.datagraph <- function(x) {
  "datagraph" %in% class(x)
}

#' @export
as.datagraph.igraph <- function(x, ...) {
  vertices <- setDT(igraph::as_data_frame(x, what = "vertices"))
  edges    <- setDT(igraph::as_data_frame(x, what = "edges"))

  if (!("id" %in% names(vertices))) vertices$id <- rownames(vertices)
  setDT(vertices, key = "id")
  # TODO: check for duplicates

  obj <- datagraph()

  add_vertices.datagraph(obj, vertices = vertices)
  add_edges.datagraph(obj, edges = edges)

  return(obj)
}


#' @export
as.datagraph.data.table <- function(x, vertices = NULL, add_missing = FALSE) {
  obj <- datagraph()

  if (is.null(vertices)) {
    vertices <- x[, .(id = union(from, to))]
  } else {
    if (is.null(vertices[["id"]])) stop("Table of vertices requires an id column")
    reqids <- x[, union(from, to)]
    misids <- setdiff(reqids, vertices[, id])
    if (length(misids)) {
      if (isTRUE(add_missing)) {
        vertices <- rbindlist(list(vertices, data.table(id = misids)), fill = TRUE)
      } else {
        stop("Some vertices are missing: ", paste0(misids, collapse = ", "))
      }
    }
  }

  add_vertices.datagraph(obj, vertices = vertices)
  add_edges.data.table(x, graph = obj)

  return(obj)
}

#' @export
as.list.datagraph <- function(x, sorted = FALSE) {
  as.list.environment(x, sorted = sorted)
}

#' @export
as.list.datagraph_edgelist <- function(x, sorted = FALSE) {
  as.list.environment(x, sorted = sorted)
}

#' @export
as.list.datagraph_vertex <- function(x, what = "all") {
  if (what == "all")      return(as.list.environment(x))
  if (what == "edges")    return(list(from = x[["from"]], to = x[["to"]]))
  if (what == "vertices") {
    obj <- as.list.environment(x)
    return(obj[-match(c("from", "to"), names(obj))])
  }
}

#' @importFrom data.table as.data.table
#' @export
as.data.table.datagraph_vertex <- function(x, what = "all") {
  obj <-
    switch(
      what,
      "all"      = list(id = x[["id"]], from = list(names(x[["from"]])), to = list(names(x[["to"]]))),
      "edges"    = list(from = c(rep(x[["id"]], length(x[["to"]])), names(x[["from"]])),
                        to   = c(names(x[["to"]]),                  rep(x[["id"]], length(x[["from"]])))),
      "vertices" = list(id = x[["id"]])
    )
  if (!is.null(x[["data"]]) & (what == "all" || what == "vertices")) {
    obj <- c(obj, as.list.environment(x[["data"]], sorted = FALSE))
  }
  return(as.data.table(obj))
}

#' @importFrom data.table as.data.table
#' @export
as.data.table.datagraph <- function(x, what = c("all", "edges", "vertices")) {
  what <- match.arg(what)

  l <- vector("list", length(x))
  for (i in ls(x, sorted = FALSE)) {
    l[[i]] <- as.data.table(x[[i]], what = what)
  }
  obj <- rbindlist(l, use.names = TRUE, fill = TRUE)
  if (what == "all" || what == "vertices") setkeyv(obj, "id")
  if (what == "edges") obj <- unique(obj)

  return(obj)
}

#' @importFrom igraph as.igraph
#' @export
as.igraph.datagraph <- function(x, add_missing = TRUE) {
  x <- as.data.table.datagraph(x, what = "all")
  edges <- unique(rbind(
    x[, .(to   = unlist(to)),   by = .(from = id)][, .(from, to)],
    x[, .(from = unlist(from)), by = .(to   = id)][, .(from, to)]
  ))
  vertices <- setnames(x[, !c("from", "to"), with = FALSE], "id", "name")
  missing_vertices <- setdiff(edges[, c(from, to)], vertices$name)
  if (length(missing_vertices)) {
    if (isTRUE(add_missing)) {
      vertices <- rbindlist(
        list(
          vertices,
          data.table(name = missing_vertices)
        ),
        use.names = TRUE,
        fill = TRUE
      )
    } else {
      edges <- edges[!(from %in% missing_vertices | to %in% missing_vertices)]
    }
  }
  igraph::graph_from_data_frame(
    edges,
    vertices = vertices,
    directed = TRUE
  )
}
