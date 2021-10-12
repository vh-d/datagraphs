#' @export
datagraph <- function() {
  graph <- new.env()
  graph[[".edges"]] <- edgelist()
  class(graph) <- c("datagraph")
  return(graph)
}

#' @export
datagraph_vertex <- function() {
  v <- new.env()
  v[["from"]] <- edgelist()
  v[["to"]]   <- edgelist()
  v[["data"]] <- new.env()
  class(v) <- c("datagraph_vertex")
  return(v)
}

#' @export
datagraph_edge <- function() {
  e <- structure(new.env(), class = "datagraph_edge")
  return(e)
}

#' @export
edgelist <- function() {
  el <- structure(new.env(), class = "datagraph_edgelist")
  return(el)
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
  obj <- datagraph()

  if (is.null(vertices)) {
    vertices <- x[, .(id = union(from, to))]
    # vertices <- vertices[!is.na(vertices)]
  }

  add_vertices.datagraph(obj, vertices = vertices)
  add_edges.data.table(x, graph = obj)

  return(obj)
}


#' @export
as.list.datagraph <- function(x, sorted = TRUE) {
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

#' @export
add_vertex.datagraph <- function(x, vertex) {
  add_vertex(vertex, graph = x)
  return(invisible(x))
}

#' @export
add_vertex.character <- function(x, graph, from = NULL, to = NULL, ...) {
  v <- datagraph_vertex()
  list2env(
    list(
      id = x,
      from = as_edgelist(from, graph = graph),
      to   = as_edgelist(to,   graph = graph),
      ...
    ),
    envir = v
  )
  .Primitive("[[<-")(graph, x, v)
  return(invisible(v))
}


#' @export
add_vertex.data.table <- function(x, graph) {
  v <- datagraph_vertex()
  .Primitive("[[<-")(v, "id", x[["id"]])
  datacols <- setdiff(names(x), c("id", "from", "to"))
  if (length(datacols)) list2env(x[, datacols, with = FALSE], v[["data"]])
  .Primitive("[[<-")(graph, v[["id"]], v)
  return(invisible(v))
}

#' @export
add_vertex.list <- function(x, graph) {
  v <- datagraph_vertex()
  .Primitive("[[<-")(v, "id", x[["id"]])
  datacols <- setdiff(names(x), c("id", "from", "to"))
  if (length(datacols)) list2env(x[, datacols, with = FALSE], v[["data"]])
  .Primitive("[[<-")(graph, v[["id"]], v)
  return(invisible(v))
}

#' @export
add_vertices.datagraph <- function(x, vertices) {
  add_vertices(vertices, graph = x)
  return(invisible(x))
}

#' @export
add_vertices.data.table <- function(x, graph) {
  vertices <- copy(x)
  vertices[, id := as.character(id)] # TODO: no need of a copy if id is character

  verticeslist <- split(vertices, by = "id")
  lapply(verticeslist, add_vertex, x = graph)
}

#' @export
V.datagraph <- function(x, sorted = FALSE) {
  ls(x, sorted = sorted)
}

vertices.datagraph <- function(x, sorted = TRUE) {
  as.list.environment(x, sorted = sorted)
}

#' @export
E.datagraph <- function(x, sorted = FALSE) {
  ls(x[[".edges"]], sorted = sorted)
}

edges.datagraph <- function(x, sorted = TRUE) {
  as.list.environment(x[[".edges"]], sorted = sorted)
}

#' @export
contains_vertex <- function(x, vertex) {
  vertex %in% ls(x, sorted = FALSE)
}


#' @export
add_edge.datagraph <- function(x, from, to, data = NULL, edge) {
  if (!missing(from))
    add_edge(from, to, data = data, graph = x) else
      add_edge(edge, graph = x)
  return(invisible(x))
}

#' @export
remove_edge.datagraph <- function(x, from, to, edge) {
  if (!missing(from))
    remove_edge(from, to, graph = x) else
      remove_edge(edge, graph = x)
  return(invisible(x))
}


add_neighbor_in <- function(v, edge) {
  fromedges <- v[["from"]]
  v2 <- edge[["from"]]
  v2id <- v2[["id"]]
  .Primitive("[[<-")(fromedges, v2id, edge)
}

add_neighbor_out <- function(v, edge) {
  toedges <- v[["to"]]
  v2 <- edge[["to"]]
  v2id <- v2[["id"]]
  .Primitive("[[<-")(toedges, v2id, edge)
}

remove_neighbor_in <- function(v, edge) {
  fromedges <- v[["from"]]
  v2 <- edge[["from"]]
  v2id <- v2[["id"]]
  rm(list = v2id, envir = fromedges)
}

remove_neighbor_out <- function(v, edge) {
  toedges <- v[["to"]]
  v2 <- edge[["to"]]
  v2id <- v2[["id"]]
  rm(list = v2id, envir = toedges)
}


add_edge_to_graph <- function(edge, graph, from = edge[["from"]], to = edge[["to"]]) {
  edgeid <- edge[["id"]]
  edgelist = graph[[".edges"]]
  edgelist[[edgeid]] <- edge
  add_neighbor_out(from, edge)
  add_neighbor_in(to, edge)
  return()
}

remove_edge_from_graph <- function(edgeid, graph, from, to) {
  edgelist = graph[[".edges"]]
  edge <- edgelist[[edgeid]]
  remove_neighbor_out(from, edge)
  remove_neighbor_in(to, edge)
  rm(list = edgeid, envir = edgelist)
  return()
}

#' @export
add_edge.datagraph_edge <- function(x, graph) {
  v1 <- x[["from"]]
  v2 <- x[["to"]]
  add_edge_to_graph(x, graph = graph, from = v1, to = v2)
  return(invisible(x))
}

#' @export
add_edge.datagraph_vertex <- function(from, to, graph, data = NULL) {
  e <- datagraph_edge()
  e[["id"]] <- sprintf("%s->%s", from[["id"]], to[["id"]])
  e[["from"]] <- from
  e[["to"]]   <- to
  if (!is.null(data)) e[["data"]] <- data
  add_edge_to_graph(e, graph = graph, from = from, to = to)
  return(e)
}

#' @export
add_edge.character <- function(from, to, graph, data = NULL) {
  e <- datagraph_edge()
  e[["id"]] <- sprintf("%s->%s", from, to)
  fv <- graph[[from]]
  if (is.null(fv)) stop(from, " vertex does not exist in the graph")
  tv <- graph[[to]]
  if (is.null(tv)) stop(to, " vertex does not exist in the graph")
  e[["from"]] <- fv
  e[["to"]]   <- tv
  if (!is.null(data)) e[["data"]] <- data
  add_edge_to_graph(e, graph = graph, from = fv, to = tv)
  return(e)
}

#' @export
remove_edge.character <- function(from, to, graph) {
  edgeid <- sprintf("%s->%s", from, to)
  from <- graph[[from]]
  to   <- graph[[to]]
  remove_edge_from_graph(edgeid, graph = graph, from = from, to = to)
  return()
}

#' @export
add_edge.list <- function(x, graph) {
  e <- datagraph_edge()
  fid <- x[["from"]]
  tid <- x[["to"]]
  eid <- sprintf("%s->%s", fid, tid)
  from <- graph[[fid]]
  to   <- graph[[tid]]
  e[["id"]]   <- eid
  e[["from"]] <- from
  e[["to"]]   <- to
  e[["data"]] <- x[["data"]]
  add_edge_to_graph(e, graph = graph, from = from, to = to)
  return(invisible(x))
}


#' @export
add_edges.datagraph <- function(x, edges) {
  add_edges(edges, graph = x)
  return(invisible(x))
}

#' @export
add_edges.data.table <- function(x, graph) {
  edges <- unique(x, by = c("from", "to"))
  if (identical(attr(edges, ".internal.selfref"), attr(x, ".internal.selfref"))) edges <- copy(x)

  edges[, from := as.character(from)]
  edges[, to   := as.character(to)]

  for (i in split(edges, by = c("from", "to"))) {
    add_edge.character(i[["from"]], i[["to"]], graph = graph)
  }

  return()
}

# expects vertex names as character
#' @export
are_adjacent.datagraph <- function(x, from, to) {
  eid <- sprintf("%s->%s", from, to)
  el <- x[[".edges"]]
  return(eid %in% names(el))
}

#' @export
are_adjacent.datagraph_vertex <- function(from, to) {
  toid <- to[["id"]]
  tovs <- names(from[["to"]])
  return(toid %in% tovs)
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
  vs <- ls(x, sorted = FALSE)
  es <- names(x[[".edges"]])
  cat(length(vs), "vertices:", head(vs, 10), "...\n")
  cat(length(es), "edges:",    head(es, 10), "...\n")
}

#' @export
print.datagraph_vertex <- function(x) {
  cat("<datagraph vertex>", x[["id"]], "\n")
  cat("edges from:", names(x[["from"]]), "\n")
  cat("edges to:",   names(x[["to"]]), "\n")
  cat("Attributes:", setdiff(ls(x[["data"]], sorted = FALSE), c("id", "from", "to")))
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
neighbors_in.datagraph_vertex <- function(x, names = FALSE) {
  if (isTRUE(names)) return(names(x[["from"]]))
  es <- x[["from"]]
  r <- vector("list", length = length(es))
  i <- 0L
  for (j in as.list.environment(es, sorted = FALSE)) {
    i <- i + 1L
    r[[i]] <- .Primitive("[[")(j, "from")
  }

  return(r)
}

#' @export
neighbors_in.datagraph <- function(x, vertices, names = FALSE, useNames = TRUE) {
  vertices <- intersect(V(x), vertices)
  unlist(lapply(mget(vertices, envir = x), neighbors_in.datagraph_vertex, names = names), use.names = useNames)
}

#' @export
neighbors_out.datagraph_vertex <- function(x, names = FALSE) {
  if (isTRUE(names)) return(names(x[["to"]]))
  es <- x[["to"]]
  r <- vector("list", length = length(es))
  i <- 0L
  for (j in as.list.environment(es, sorted = FALSE)) {
    i <- i + 1L
    r[[i]] <- .Primitive("[[")(j, "to")
  }

  return(r)
}

#' @export
neighbors_out.datagraph <- function(x, vertices, names = FALSE, useNames = TRUE) {
  vertices <- intersect(V(x), vertices)
  unlist(lapply(mget(vertices, envir = x), neighbors_out.datagraph_vertex, names = names), use.names = useNames)
}

#' @export
neighbors.datagraph <- function(x, vertices, mode = "all", names = FALSE) {
  switch (mode,
          "in"  = neighbors_in(x, vertices, names = names),
          "out" = neighbors_out(x, vertices, names = names),
          "all" =
            union(
              neighbors_in(x, vertices, names = names),
              neighbors_out(x, vertices, names = names)
            )
  )
}


#' @export
neighborhood.datagraph <- function(x, vertices, order = 1000, mode = "all", names = FALSE) {
  newvisits <- vertices
  visited <- newvisits
  steps <- 0L
  while (steps < order && length(newvisits) > 0L) {
    newvisits <- neighbors(x, vertices = newvisits, mode = mode, names = names)
    newvisits <- setdiff(newvisits, visited)
    visited <- c(visited, newvisits)
    steps <- steps + 1L
  }

  return(visited)
}

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

#' @export
relink_edge <- function(e, graph, from = e[["from"]], to = e[["to"]]) {
  oldfrom <- e[["from"]]
  oldfromid <- oldfrom[["id"]]
  newfrom <- from
  newfromid <- from[["id"]]

  oldto <- e[["to"]]
  oldtoid <- oldto[["id"]]
  newto <- to
  newtoid <- to[["id"]]

  oldid <- e[["id"]]
  newid <- sprintf("%s->%s", newfromid, newtoid)

  e[["from"]] <- newfrom
  e[["to"]] <- newto

  remove(list = oldid, envir = graph[[".edges"]])
  graph[[".edges"]][[newid]] <- e

  remove(list = oldfromid, envir = oldto[["from"]])
  newto[["from"]][[newfromid]] <- e

  remove(list = oldtoid, envir = oldfrom[["to"]])
  newfrom[["to"]][[newtoid]] <- e

  return(invisible(e))
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
