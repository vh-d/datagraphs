#' @export
datagraph <- function() {
  obj <- new.env()
  class(obj) <- c("datagraph")
  return(obj)
}

#' @export
datagraph_vertex <- function() {
  obj <- new.env()
  class(obj) <- c("datagraph_vertex")
  return(obj)
}

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


all.equal.datagraph_vertex <- function(current, target) {
  all.equal.environment(current, target)
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
    try(graph[[i$from[1]]][["to"]] <- i[["to"]], silent = TRUE) # TODO: more careful handling of non-existent vertices
  }

  for (j in lto) {
    try(graph[[j$to[1]]][["from"]] <- j[["from"]], silent = TRUE)
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
  cat(length(x), "vertices:", head(ls(x, sorted = FALSE), 10))
}

#' @export
print.datagraph_vertex <- function(x) {
  cat("<datagraph vertex>", x[["id"]], "\n")
  cat("edges from:", x[["from"]], "\n")
  cat("edges to:",   x[["to"]], "\n")
  cat("Attributes:", setdiff(ls(x, sorted = FALSE), c("id", "from", "to")))
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
neighbors_out.datagraph_vertex <- function(vertex) {
  vertex[["to"]]
}

#' @export
neighbors_out.datagraph <- function(graph, vertices, useNames = TRUE) {
  unlist(lapply(mget(vertices, envir = graph), neighbors_out.datagraph_vertex), use.names = useNames)
}

#' @export
neighbors.datagraph <- function(graph, vertices, mode = c("in", "out", "all")) {
  switch (mode,
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
  newvertex <- datagraph_vertex()
  for (i in ls(vertex, sorted = FALSE)) {
    assign(i, value = vertex[[i]], envir = newvertex) # TODO: deep copy for objects passed by reference
  }
  return(newvertex)
}


#' @export
copy_graph <- function(graph, as_list = FALSE) {
  if (!isTRUE(as_list)) {
    newgraph <- datagraph()
    for (i in ls(graph, sorted = FALSE)) {
      new_vertex <- copy_vertex(graph[[i]])
      parent.env(new_vertex) <- newgraph
      assign(i, new_vertex, envir = newgraph)
    }
  } else {
    newgraph <- vector("list", length(graph))
    for (i in ls(graph, sorted = FALSE)) {
      new_vertex <- copy_vertex(graph[[i]])
      newlist[[i]] <- new_vertex
    }
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

#' @importFrom igraph union
#' @export
union.datagraph <- function(...) {
  union_list_of_graph(list(...))
}

#' @export
union_list_of_graphs <- function(x) {
  newgraph <- datagraph()
  for (i in x) {
    coi <- copy_graph(i, as_list = TRUE)
    list2env(coi, newgraph)
  }
  return(newgraph)
}
