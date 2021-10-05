#' @export
datagraph <- function() {
  graph <- new.env()
  edges <- new.env(parent = graph)
  # assign(".edges", edges, envir = graph, inherits = FALSE)
  class(graph) <- c("datagraph")
  return(graph)
}

datagraph_vertex <- function() {
  obj <- new.env()
  class(obj) <- c("datagraph_vertex")
  return(obj)
}

#' @export
vertex.default <- function(graph = emptyenv()) {
  v <- datagraph_vertex()
  v[["from"]] <- new.env(parent = v)
  v[["to"]]   <- new.env(parent = v)
  return(v)
}

#' @export
vertex.character <- function(x, from = NULL, to = NULL, ..., graph = emptyenv()) {
  v <- vertex.default(graph = graph)
  list2env(list(id = x, from = from, to = to, data = list(...)), envir = v)
  return(v)
}

#' @export
vertex.list <- function(x, graph = emptyenv()) {
  v <- vertex.default(graph = graph)
  list2env(x, envir = v)
  return(v)
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
check.datagraph <- function(x) {
  return(TRUE)
}


#' @export
as.datagraph.igraph <- function(x, ...) {
  vertices <- setDT(igraph::as_data_frame(x, what = "vertices"))
  edges    <- setDT(igraph::as_data_frame(x, what = "edges"))

  if (!("id" %in% names(vertices))) vertices$id <- rownames(vertices)
  setDT(vertices, key = "id")
  # TODO: check for duplicates

  obj <- new.env()
  class(obj) <- "datagraph"

  add_vertices.datagraph(obj, vertices = vertices)
  add_edges.datagraph(obj, edges = edges)

  return(obj)
}

#' @export
as.datagraph.data.table <- function(x, vertices = NULL) {
  obj <- new.env()
  class(obj) <- "datagraph"

  if (is.null(vertices)) {
    vertices <- x[, .(id = union(from, to))]
    # vertices <- vertices[!is.na(vertices)]
  }

  add_vertices.datagraph(obj, vertices = vertices)
  add_edges.datagraph(obj, edges = x)

  return(obj)
}


#' @export
as.list.datagraph <- function(x) {
  as.list.environment(x)
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
      "all"      = list(id = x[["id"]], from = list(x[["from"]]), to = list(x[["to"]])),
      "edges"    = list(from = c(rep(x[["id"]], length(x[["to"]])), x[["from"]]),
                        to   = c(x[["to"]],                         rep(x[["id"]], length(x[["from"]])))),
      "vertices" = list(id = x[["id"]])
    )
  if (what == "all" || what == "vertices") {
    other_data <- setdiff(names(x), c("id", "from", "to"))
    obj <- c(obj, mget(other_data, envir = x))
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
as.igraph.datagraph <- function(x) {
  x <- as.data.table.datagraph(x)
  igraph::graph_from_data_frame(
    d = x[, .(to = unlist(to)), by = .(from = id)],
    vertices = setnames(x[, !c("from", "to"), with = FALSE], "id", "name"),
    directed = TRUE
  )
}

#' @export
add_vertex <- function(x, vertex) {
  obj <- new.env(parent = x)
  id <- as.character(vertex[["id"]])
  list2env(vertex, obj)

  obj[["to"]]   <- character()
  obj[["from"]] <- character()

  class(obj) <-  "datagraph_vertex"
  assign(id, obj, envir = x)

  return(invisible(TRUE))
}

#' @export
add_vertices.datagraph <- function(x, vertices) {
  add_vertices(vertices, graph = x)

  return(invisible(TRUE))
}

#' @export
add_vertices.data.table <- function(x, graph) {
  vertices <- setDT(copy(x))
  verticeslist <- split(vertices, by = "id")
  lapply(verticeslist, add_vertex, x = graph)
}

#' @export
V.datagraph <- function(x, sorted = FALSE) {
  ls(x, sorted = sorted)
}

#' @export
E.datagraph <- function(x, sorted = FALSE) {
  as.data.table(x, what = "edges")
}

#' @export
contains_vertex <- function(x, vertex) {
  vertex %in% ls(x, sorted = FALSE)
}

#' @export
add_edge.datagraph <- function(x, edge) {
  vf <- x[[edge$from]]
  vt <- x[[edge$to]]
  vf[["to"]] <- union(vf[["to"]], edge$to)
  vt[["from"]] <- union(vf[["from"]], edge$from)
}

#' @export
add_edges.datagraph <- function(x, edges) {
  add_edges(edges, graph = x)
  return(invisible(x))
}

#' @export
add_edges.data.table <- function(x, graph) {
  edges <- setDT(copy(x))
  edges[, from := as.character(from)]
  edges[, to   := as.character(to)]

  lfrom <- split(edges, by = "from")
  lto   <- split(edges, by = "to")

  for (i in lfrom) {
    v <- graph[[i$from[1]]]
    if (!is.null(v)) v[["to"]] <- i[["to"]]
  }

  for (j in lto) {
    v <- graph[[j$to[1]]]
    if (!is.null(v)) v[["from"]] <- j[["from"]]
  }
}

#' @export
are_adjacent.datagraph <- function(x, vertex1, vertex2) {
  vertex2 %in% x[[vertex1]]$to || vertex2 %in% x[[vertex1]]$from
}

#' @export
remove_vertex.datagraph <- function(x, vertex, ...) {
  from = x[[vertex]]$from
  to   = x[[vertex]]$to
  if (length(to))   remove_edges(x, data.table(from = vertex, to = to))
  if (length(from)) remove_edges(x, data.table(from = from, to = vertex))
  remove(list = vertex, envir = x)

  return(invisible(x))
}

#' @export
remove_vertices.datagraph <- function(x, vertices, ...) {
  lapply(vertices, remove_vertex.datagraph, x = x)

  return(invisible(x))
}

#' @export
remove_edge.datagraph <- function(x, edge) {
  vertex_from <- x[[edge$from]]
  vertex_to   <- x[[edge$to]]
  vertex_from$to <- setdiff(vertex_from$to, edge$to)
  vertex_to$from <- setdiff(vertex_to$from, edge$from)

  return(invisible(x))
}

#' @export
remove_edges.datagraph <- function(x, edges) {
  if (nrow(edges))
    for (i in seq_len(nrow(edges))) {
      remove_edge.datagraph(x, edges[i, ])
    }

  return(invisible(x))
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
  iexp <- if (missing(i)) quote(TRUE) else substitute(i)
  jexp <- if (missing(j)) quote(data.table(id = id, from = list(from), to = list(to))) else substitute(j)

  i <- tryCatch(i, error = function(e) NULL )
  if (!is.character(i)) {
    ires <- sapply(as.list.environment(x), eval, expr = iexp)
    ids <- names(which(ires))
  } else {
    ids <- i
  }

  rbindlist(lapply(mget(x = ids, envir = x), eval, expr = jexp))
}

#' @export
neighbors_in.datagraph_vertex <- function(vertex) {
  vertex[["from"]]
}

#' @export
neighbors_in.datagraph <- function(x, vertices, useNames = TRUE) {
  vertices <- intersect(V(x), vertices)
  unlist(lapply(mget(vertices, envir = x), neighbors_in.datagraph_vertex), use.names = useNames)
}

#' @export
neighbors_out.datagraph_vertex <- function(vertex) {
  vertex[["to"]]
}

#' @export
neighbors_out.datagraph <- function(x, vertices, useNames = TRUE) {
  vertices <- intersect(V(x), vertices)
  unlist(lapply(mget(vertices, envir = x), neighbors_out.datagraph_vertex), use.names = useNames)
}

#' @export
neighbors.datagraph <- function(x, vertices, mode = "all") {
  switch (mode,
    "in"  = neighbors_in(x, vertices),
    "out" = neighbors_out(x, vertices),
    "all" =
      union(
        neighbors_in(x, vertices),
        neighbors_out(x, vertices)
      )
  )
}


#' @export
neighborhood.datagraph <- function(x, vertices, order = 1000, mode = "all") {
  newvisits <- vertices
  visited <- newvisits
  steps <- 0L
  while (steps < order && length(newvisits) > 0L) {
    newvisits <- neighbors(x, vertices = newvisits, mode = mode)
    newvisits <- setdiff(newvisits, visited)
    visited <- c(visited, newvisits)
    steps <- steps + 1L
  }

  return(visited)
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
    newgraph <- as.list(graph)
    i <- 0L
    # iterate and replace by copies
    for (v in newgraph) {
      i <- i + 1L
      newv <- copy_vertex(v)
      newgraph[[i]] <- newv
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


#' @export
subset.datagraph <- function(x, subset) {
  subset <- intersect(V(x), subset)
  subgraph <- list2env(mget(subset, envir = x, mode = "environment"))
  class(subgraph) <- c("datagraph_subgraph", "datagraph")

  return(subgraph)
}


reconnect_graph <- function(graph) {
  for (v in as.list(graph)) {
    vid <- v[["id"]]
    for (i in mget(v$from, envir = graph, ifnotfound = NA_character_, inherits = FALSE)) {
      if (!is.na(i)) i$to <- union(i$to, vid)
    }
    for (j in mget(v$to, envir = graph, ifnotfound = NA_character_, inherits = FALSE)) {
      if (!is.na(j)) j$from <- union(j$from, vid)
    }
  }
  return(invisible(graph))
}

#' @importFrom igraph union
#' @export
union.datagraph <- function(...) {
  union_list_of_graphs(list(...))
}

#' @export
union_list_of_graphs <- function(x) {
  newgraph <- datagraph()
  for (i in x) {
    coi <- copy_graph(i, as_list = TRUE)
    list2env(coi, newgraph)
  }
  reconnect_graph(newgraph)
  return(newgraph)
}
