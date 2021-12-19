#' @param vertex id of a vertex
#' @rdname add_vertices
#' @export
add_vertex.datagraph <- function(x, vertex) {
  add_vertex(vertex, graph = x)
  return(invisible(x))
}

#' @param graph datagraph object
#' @param from vector of vertices to connect from
#' @param to vector of vertices to connect from
#' @rdname add_vertices
#' @export
add_vertex.character <- function(x, graph, from = NULL, to = NULL, ...) {
  v <- datagraph_vertex()
  .Primitive("[[<-")(v, "id", x)
  list2env(list(...), v[["data"]])
  .Primitive("[[<-")(graph, x, v)
  for (i in from) add_edge(i, to = x, graph = graph)
  for (j in to) add_edge(x, to = j, graph = graph)
  return(invisible(v))
}


#' @rdname add_vertices
#' @export
add_vertex.data.table <- function(x, graph) {
  v <- datagraph_vertex()
  .Primitive("[[<-")(v, "id", x[["id"]])
  datacols <- setdiff(names(x), c("id", "from", "to"))
  if (length(datacols)) list2env(x[, datacols, with = FALSE], v[["data"]])
  .Primitive("[[<-")(graph, v[["id"]], v)
  return(invisible(v))
}

#' @rdname add_vertices
#' @export
add_vertex.list <- function(x, graph) {
  v <- datagraph_vertex()
  .Primitive("[[<-")(v, "id", x[["id"]])
  datacols <- setdiff(names(x), c("id", "from", "to"))
  if (length(datacols)) list2env(x[, datacols, with = FALSE], v[["data"]])
  .Primitive("[[<-")(graph, v[["id"]], v)
  return(invisible(v))
}

#' @param vertices vector of new ids
#' @rdname add_vertices
#' @export
add_vertices.datagraph <- function(x, vertices) {
  add_vertices(vertices, graph = x)
  return(invisible(x))
}

#' @rdname add_vertices
#' @export
add_vertices.data.table <- function(x, graph) {
  vertices <- copy(x)
  vertices[, id := as.character(id)] # TODO: no need of a copy if id is character

  verticeslist <- split(vertices, by = "id")
  lapply(verticeslist, add_vertex, x = graph)
}



#' @rdname vertices
#' @export
V.datagraph <- function(x, sorted = FALSE) {
  ls(x, sorted = sorted)
}

#' @rdname vertices
#' @export
vertices.datagraph <- function(x, sorted = FALSE) {
  as.list.environment(x, sorted = sorted)
}

#' @rdname edges
#' @export
E.datagraph <- function(x, sorted = FALSE) {
  ls(x[[".edges"]], sorted = sorted)
}

#' @rdname edges
#' @export
edges.datagraph <- function(x, sorted = FALSE) {
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

#' @param graph a datagraph object
#' @rdname add_edges
#' @export
add_edge.datagraph_edge <- function(x, graph) {
  v1 <- x[["from"]]
  v2 <- x[["to"]]
  add_edge_to_graph(x, graph = graph, from = v1, to = v2)
  return(invisible(x))
}

#' @param from character; id of the start node
#' @param to character; id of the end node
#' @param data list of data attributes
#'
#' @rdname add_edges
#' @export
add_edge.datagraph_vertex <- function(from, to, graph, data = NULL) {
  e <- datagraph_edge()
  e[["id"]] <- sprintf("%s->%s", from[["id"]], to[["id"]])
  e[["from"]] <- from
  e[["to"]]   <- to
  if (!is.null(data)) list2env(data, e[["data"]])
  add_edge_to_graph(e, graph = graph, from = from, to = to)
  return(e)
}

#' @rdname add_edges
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
  if (!is.null(data)) e[["data"]] <- list2env(as.list(data))
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

#' @rdname add_edges
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
  if (!is.null(x[["data"]])) list2env(x[["data"]], e[["data"]])
  add_edge_to_graph(e, graph = graph, from = from, to = to)
  return(invisible(x))
}


#' @rdname add_edges
#' @export
add_edges.datagraph <- function(x, edges) {
  add_edges(edges, graph = x)
  return(invisible(x))
}

#' @rdname add_edges
#' @export
add_edges.data.table <- function(x, graph) {
  edges <- unique(x, by = c("from", "to"))
  if (identical(attr(edges, ".internal.selfref"), attr(x, ".internal.selfref"))) edges <- copy(x)

  edges[, from := as.character(from)]
  edges[, to   := as.character(to)]

  for (i in split(edges, by = c("from", "to"))) {
    add_edge.character(i[["from"]], i[["to"]], data = i[, !c("from", "to")], graph = graph)
  }

  return()
}

#' @export
remove_vertex.datagraph <- function(x, vertex, ...) {
  remove_vertex(vertex, graph = x)
  return(invisible(x))
}

#' @export
remove_vertex.character <- function(x, graph) {
  from = graph[[x]][["from"]]
  to   = graph[[x]][["to"]]
  if (length(from)) remove_edges(from, graph = graph)
  if (length(to)) remove_edges(to, graph = graph)
  remove(list = x, envir = graph)

  return(invisible(x))
}

#' @export
remove_edges.datagraph_edgelist <- function(x, graph) {
  el <- as.list.environment(x)
  for (i in el) {
    remove_edge_from_graph(i[["id"]], graph = graph, from = i[["from"]], to = i[["to"]])
  }
  return()
}

#' @export
remove_vertices.datagraph <- function(x, vertices, ...) {
  lapply(vertices, remove_vertex.datagraph, x = x)

  return(invisible(x))
}


#' @export
remove_edges.datagraph <- function(x, edges) {
  remove_edges(edges, graph = x)
  return(invisible(x))
}

#' @export
remove_edges.data.table <- function(x, graph) {
  if (nrow(x)) {
    el <- split(x, by = c("from", "to"))
    for (i in el) {
      remove_edge.character(i[["from"]], i[["to"]], graph = graph)
    }
  }

  return(invisible(x))
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
  e[["id"]] <- newid

  remove(list = oldid, envir = graph[[".edges"]])
  graph[[".edges"]][[newid]] <- e

  remove(list = oldfromid, envir = oldto[["from"]])
  newto[["from"]][[newfromid]] <- e

  remove(list = oldtoid, envir = oldfrom[["to"]])
  newfrom[["to"]][[newtoid]] <- e

  return(invisible(e))
}

#' @export
`[.datagraph_vertex` <- function(x, i) {
  de <- x[["data"]]
  .Primitive("[[")(de, i)
}

#' @export
`[<-.datagraph_vertex` <- function(x, i, value) {
  de <- x[["data"]]
  .Primitive("[[<-")(de, i, value)
  return(x)
}

#' @export
`$.datagraph_vertex` <- `[.datagraph_vertex`

#' @export
`$<-.datagraph_vertex` <- `[<-.datagraph_vertex`

#' @export
`[.datagraph_edge` <- function(x, i) {
  de <- x[["data"]]
  .Primitive("[[")(de, i)
}

#' @export
`[<-.datagraph_edge` <- function(x, i, value) {
  de <- x[["data"]]
  .Primitive("[[<-")(de, i, value)
  return(x)
}

#' @export
`$.datagraph_edge` <- `[.datagraph_edge`

#' @export
`$<-.datagraph_edge` <- `[<-.datagraph_edge`
