#' @export
`%vin%` <- function(x, y) {
  sapply(y, `%chin%`, x = x, USE.NAMES = FALSE)
}

#' Check/test a datagraph object's consistency
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @export
check <- function(x, ...) {
  UseMethod("check", x)
}


#' Constructors of datagraph objects
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @export
as.datagraph <- function(x, ...) {
  UseMethod("as.datagraph", x)
}


#' @title Add vertices
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @rdname add_vertices
#' @export
add_vertex <- function(x, ...) {
  UseMethod("add_vertex", x)
}


#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @rdname add_vertices
#' @export
add_vertices <- function(x, ...) {
  UseMethod("add_vertices", x)
}


#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#' @rdname vertices
#' @export
V <- function(x, ...) {
  UseMethod("V", x)
}

#' List all vertices in a graph
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#' @rdname vertices
#' @export
vertices <- function(x, ...) {
  UseMethod("vertices", x)
}

#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#' @rdname edges
#' @export
E <- function(x, ...) {
  UseMethod("E", x)
}

#' List all edges in a graph
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#' @rdname edges
#' @export
edges <- function(x, ...) {
  UseMethod("edges", x)
}


#' Remove vertices from a x
#'
#' @param x see the S3 methods
#' @param vertex a vertex object
#' @param ... args passed to the S3 methods
#'
#' @export
remove_vertex <- function(x, vertex, ...) {
  UseMethod("remove_vertex", x)
}


#' Remove vertices from a graph
#'
#' @param x see the S3 methods
#' @param vertices a vector of vertices' ids
#' @param ... passed to individual methods
#'
#' @export
remove_vertices <- function(x, vertices, ...) {
  UseMethod("remove_vertices", x)
}

#' Add an edge
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#' @rdname add_edges
#' @export
add_edge <- function(x, ...) {
  UseMethod("add_edge", x)
}


#' Add edges (between vertices) to a graph
#'
#' @param x see S3 methods
#' @param ... passed to individual methods
#'
#' @export
add_edges <- function(x, ...) {
  UseMethod("add_edges", x)
}

#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @rdname remove_edges
#' @export
remove_edge <- function(x, ...) {
  UseMethod("remove_edge", x)
}


#' Remove edges between vertices
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @export
remove_edges <- function(x, ...) {
  UseMethod("remove_edges", x)
}

#' Test if two vertices are adjacent (direct neighbors)
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @export
are_adjacent <- function(x, ...) {
  UseMethod("are_adjacent", x)
}


#' @description neighbors - adjacent vertices
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @rdname neighborhood
#' @export
neighbors <- function(x, ...) {
  UseMethod("neighbors", x)
}

#' @description neighbors_in - adjacent vertices following incoming edges
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @rdname neighborhood
#' @export
neighbors_in <- function(x, ...) {
  UseMethod("neighbors_in", x)
}


#' @description neighbors_out - adjacent vertices following outgoing edges
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @rdname neighborhood
#' @export
neighbors_out <- function(x, ...) {
  UseMethod("neighbors_out", x)
}

#' @title Adjacent vertices
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @rdname neighborhood
#' @export
neighborhood <- function(x, ...) {
  UseMethod("neighborhood", x)
}

#' Test that graph contains cycles
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @export
detect_cycles <- function(x, ...) {
  UseMethod("detect_cycles", x)
}

#' Collapse vertices
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @export
collapse_vertices <- function(x, ...) {
  UseMethod("collapse_vertices", x)
}


#' Make a copy
#'
#' @param x see the S3 methods
#' @param ... args passed to the S3 methods
#'
#' @export
copy_of <- function(x, ...) {
  UseMethod("copy_of", x)
}


#' Run a function for each member.
#'
#' @param x see the S3 methods
#' @param f function
#' @param ... args passed to the S3 methods
#'
#' @export
foreach <- function(x, f, ...) {
  UseMethod("foreach", x)
}

