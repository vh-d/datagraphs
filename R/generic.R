#' @export
`%vin%` <- function(x, y) {
  sapply(y, `%chin%`, x = x, USE.NAMES = FALSE)
}

#' Title
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
check <- function(graph, ...) {
  UseMethod("check", graph)
}


#' Convert to datagraph
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as.datagraph <- function(graph, ...) {
  UseMethod("as.datagraph", graph)
}



#' Title
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_vertices <- function(graph, ...) {
  UseMethod("add_vertices", graph)
}

V <- function(graph, ...) {
  UseMethod("V", graph)
}

#' Remove vertices from a graph
#'
#' @param graph
#' @param vertex
#' @param ...
#'
#' @return
#' @export
remove_vertex <- function(graph, vertex, ...) {
  UseMethod("remove_vertex", graph)
}


#' Remove vertices from a graph
#'
#' @param graph
#' @param vertices
#' @param ...
#'
#' @return
#' @export
remove_vertices <- function(graph, vertices, ...) {
  UseMethod("remove_vertices", graph)
}


#' Title
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_edges <- function(graph, ...) {
  UseMethod("add_edges", graph)
}

#' Title
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
remove_edges <- function(graph, ...) {
  UseMethod("remove_edges", graph)
}

#' Title
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
are_adjacent <- function(graph, ...) {
  UseMethod("are_adjacent", graph)
}


#' Title
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
neighbors <- function(graph, ...) {
  UseMethod("neighbors", graph)
}

#' Title
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
neighbors_in <- function(graph, ...) {
  UseMethod("neighbors_in", graph)
}


#' Title
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
neighbors_out <- function(graph, ...) {
  UseMethod("neighbors_out", graph)
}

#' Title
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
neighborhood <- function(graph, ...) {
  UseMethod("neighborhood", graph)
}

#' Title
#'
#' @param graph
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
detect_cycles <- function(graph, ...) {
  UseMethod("detect_cycles", graph)
}




