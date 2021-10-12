#' @export
`%vin%` <- function(x, y) {
  sapply(y, `%chin%`, x = x, USE.NAMES = FALSE)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
check <- function(x, ...) {
  UseMethod("check", x)
}


#' Convert to datax
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
as.datagraph <- function(x, ...) {
  UseMethod("as.datagraph", x)
}

#' @param x
#'
#' @param ...
#'
as_vertex <- function(x, ...) {
  UseMethod("as_vertex", x)
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
add_vertex <- function(x, ...) {
  UseMethod("add_vertex", x)
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
add_vertices <- function(x, ...) {
  UseMethod("add_vertices", x)
}


#' @export
V <- function(x, ...) {
  UseMethod("V", x)
}

#' @export
vertices <- function(x, ...) {
  UseMethod("vertices", x)
}


#' @export
E <- function(x, ...) {
  UseMethod("E", x)
}

#' @export
edges <- function(x, ...) {
  UseMethod("edges", x)
}


#' Remove vertices from a x
#'
#' @param x
#' @param vertex
#' @param ...
#'
#' @return
#' @export
remove_vertex <- function(x, vertex, ...) {
  UseMethod("remove_vertex", x)
}


#' Remove vertices from a graph
#'
#' @param x
#' @param vertices
#' @param ...
#'
#' @return
#' @export
remove_vertices <- function(x, vertices, ...) {
  UseMethod("remove_vertices", x)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_edge <- function(x, ...) {
  UseMethod("add_edge", x)
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_edges <- function(x, ...) {
  UseMethod("add_edges", x)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
remove_edge <- function(x, ...) {
  UseMethod("remove_edge", x)
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
remove_edges <- function(x, ...) {
  UseMethod("remove_edges", x)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
are_adjacent <- function(x, ...) {
  UseMethod("are_adjacent", x)
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
neighbors <- function(x, ...) {
  UseMethod("neighbors", x)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
neighbors_in <- function(x, ...) {
  UseMethod("neighbors_in", x)
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
neighbors_out <- function(x, ...) {
  UseMethod("neighbors_out", x)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
neighborhood <- function(x, ...) {
  UseMethod("neighborhood", x)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
detect_cycles <- function(x, ...) {
  UseMethod("detect_cycles", x)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
collapse_vertices <- function(x, ...) {
  UseMethod("collapse_vertices", x)
}


#' Make a copy
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
copy_of <- function(x, ...) {
  UseMethod("copy_of", x)
}

