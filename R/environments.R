#' @export
datagraph <- function() {
  obj <- new.env()
  class(obj) <- c("datagraph")

  return(obj)
}

#' @export
all.equal.datagraph <- function(current, target) {
  res <- vector("character", size = 1)
  if (!setequal(ls(current, sorted = FALSE), ls(target, sorted = FALSE))) res[[1]] <- "Different set of vertices"

  if (all(is.na(res))) return(TRUE) else return(res)
}


#' @export
check.datagraph <- function(graph) {
  return(TRUE)
}


#' @export
as.datagraph.igraph <- function(graph, ...) {
  vertices <- setDT(igraph::as_data_frame(graph, what = "vertices"))
  edges    <- setDT(igraph::as_data_frame(graph, what = "edges"))

  if (!("id" %in% names(vertices))) vertices$id <- rownames(vertices)
  setDT(vertices, key = "id")
  # TODO: check for duplicates

  obj <- new.env()
  class(obj) <- "datagraph"

  add_vertices.datagraph(graph = obj, vertices = vertices)
  add_edges.datagraph(graph = obj, edges = edges)

  return(obj)
}

#' @export
as.datagraph.data.table <- function(graph, vertices = NULL) {
  obj <- new.env()
  class(obj) <- "datagraph"

  if (is.null(vertices)) {
    vertices <- graph[, .(id = union(from, to))]
  }

  add_vertices.datagraph(graph = obj, vertices = vertices)
  add_edges.datagraph(graph = obj, edges = graph)

  return(obj)
}


#' @export
as.list.datagraph <- function(x) {
  as.list.environment(x)
}

#' @export
as.list.datagraph_vertex <- function(x) {
  as.list.environment(x)
}

#' @importFrom data.table as.data.table
#' @export
as.data.table.datagraph_vertex <- function(x) {
  other_data <- setdiff(names(x), c("id", "from", "to"))
  obj <- data.table(id = x[["id"]], from = list(x[["from"]]), to = list(x[["to"]]))
  if (length(other_data)) {
    for (d in other_data) {
      dvalue <- x[[d]]
      obj[, (d) := if (is.atomic(dvalue)) dvalue else list(dvalue)]
    }
  }

  obj[]
}

#' @importFrom data.table as.data.table
#' @export
as.data.table.datagraph <- function(graph) {
  l <- vector("list", length(graph))
  for (i in ls(graph, sorted = FALSE)) {
    l[[i]] <- as.data.table(graph[[i]])
  }
  setkey(rbindlist(l), id)
}

#' @importFrom igraph as.igraph
#' @export
as.igraph.datagraph <- function(graph) {
  x <- as.data.table.datagraph(graph)
  igraph::graph_from_data_frame(
    d = x[, .(to = unlist(to)), by = .(from = id)],
    vertices = setnames(x[, !c("from", "to"), with = FALSE], "id", "name"),
    directed = TRUE
  )
}

#' @export
add_vertex <- function(graph, vertex) {
  obj <- new.env(parent = graph)
  id <- as.character(vertex[["id"]])
  list2env(vertex, obj)

  obj[["to"]]   <- character()
  obj[["from"]] <- character()

  class(obj) <-  "datagraph_vertex"
  assign(id, obj, envir = graph)

  return(invisible(TRUE))
}

#' @export
add_vertices.datagraph <- function(graph, vertices) {
  vertices <- setDT(copy(vertices))
  # vertices[, add_vertex(graph, vertex = .SD, id = .BY[["id"]]), by = "id"]
  verticeslist <- split(vertices, by = "id")
  lapply(verticeslist, add_vertex, graph = graph)

  return(invisible(TRUE))
}

#' @export
contains_vertex <- function(graph, vertex) {
  vertex %in% ls(graph, sorted = FALSE)
}


#' @export
add_edges.datagraph <- function(graph, edges) {
  edges <- setDT(copy(edges))
  edges[, from := as.character(from)]
  edges[, to   := as.character(to)]

  lfrom <- split(edges, by = "from")
  lto   <- split(edges, by = "to")

  for (i in lfrom) {
    graph[[i$from[1]]][["to"]] <- i[["to"]]
  }

  for (j in lto) {
    graph[[j$to[1]]][["from"]] <- j[["from"]]
  }

  return(invisible(graph))
}

#' @export
are_adjacent.datagraph <- function(graph, vertex1, vertex2) {
  vertex2 %in% graph[[vertex1]]$to || vertex2 %in% graph[[vertex1]]$from
}

#' @export
remove_vertex.datagraph <- function(graph, vertex, ...) {
  from = graph[[vertex]]$from
  to   = graph[[vertex]]$to
  if (length(to))   remove_edges(graph, data.frame(from = vertex, to = to))
  if (length(from)) remove_edges(graph, data.frame(from = from, to = vertex))
  remove(list = vertex, envir = graph)

  return(invisible(graph))
}

#' @export
remove_edge.datagraph <- function(graph, edge) {
  vertex_from <- graph[[edge$from]]
  vertex_to   <- graph[[edge$to]]
  vertex_from$to <- setdiff(vertex_from$to, edge$to)
  vertex_to$from <- setdiff(vertex_to$from, edge$from)

  return(invisible(graph))
}

#' @export
remove_edges.datagraph <- function(graph, edges) {
  if (nrow(edges))
    for (i in seq_len(nrow(edges))) {
      remove_edge.datagraph(graph, edges[i, ])
    }

  return(invisible(graph))
}

#' @export
print.datagraph <- function(x, ...) {
  cat("<datagraph>\n")
  cat(length(x), "vertices")
}

#' @export
print.datagraph_vertex <- function(x) {
  cat("<datagraph vertex>", x[["id"]], "\n")
  # cat("edges from: ", x[["from"]][1:x[[".from_len"]]], "\n")
  # cat("edges to: ",   x[["to"]][1:x[[".to_len"]]], "\n")
  cat("edges from: ", x[["from"]], "\n")
  cat("edges to: ",   x[["to"]], "\n")
  # cat("edges to: ", x[["to"]])
}


#' @export
`[.datagraph` <- function(x, i, j) {
  iexp <- substitute(i)
  jexp <- substitute(j)

  ires <- sapply(as.list.environment(x), eval, expr = iexp)
  ids <- names(which(ires))

  rbindlist(lapply(mget(x = ids, envir = x), eval, expr = jexp))
}

#' @export
neighbors_in.datagraph_vertex <- function(vertex) {
  vertex[["from"]]
}

#' @export
neighbors_in.datagraph <- function(graph, vertices, useNames = TRUE) {
  unlist(lapply(mget(vertices, envir = graph), neighbors_in.datagraph_vertex), use.names = useNames)
}

#' @export
neighbors_outdatagraph_vertex <- function(vertex) {
  vertex[["to"]]
}

#' @export
neighbors_out.datagraph <- function(graph, vertices, useNames = TRUE) {
  unlist(lapply(mget(vertices, envir = graph), neighbors_out.datagraph_vertex), use.names = useNames)
}

#' @export
neighbors.datagraph <- function(graph, vertices, type = c("in", "out", "all")) {
  switch (type,
    "in"  = neighbors_in(graph, vertices),
    "out" = neighbors_out(graph, vertices),
    "all" =
      union(
        neighbors_in(graph, vertices),
        neighbors_out(graph, vertices)
      )
  )
}


#' @export
copy_vertex <- function(vertex) {
  newobj <- new.env()
  for (i in ls(vertex, sorted = FALSE)) {
    assign(i, value = vertex[[i]]) # TODO: deep copy for objects passed by reference
  }
  class(newobj) <- "datagraph_vertex"

  return(newobj)
}


#' @export
copy_graph <- function(graph) {
  newgraph <- datagraph()
  for (i in ls(graph, sorted = FALSE)) {
    new_vertex <- copy_vertex(graph[[i]])
    parent.env(new_vertex) <- newgraph
    assign(i, new_vertex, envir = newgraph)
  }

  return(newgraph)
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
