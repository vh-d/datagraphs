#' @rdname neighborhood
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


#' @rdname neighborhood
#' @param names logical; when TRUE only names of the vertices are returned, when FALSE -> vertex objects are returned
#' @param useNames logical; when TRUE, a named list/vector is returned
#' @export
neighbors_in.datagraph <- function(x, vertices, names = FALSE, useNames = TRUE) {
  vertices <- intersect(V(x), vertices)
  unlist(lapply(mget(vertices, envir = x), neighbors_in.datagraph_vertex, names = names), use.names = useNames)
}

#' @rdname neighborhood
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

#' @rdname neighborhood
#' @export
neighbors_out.datagraph <- function(x, vertices, names = FALSE, useNames = TRUE) {
  vertices <- intersect(V(x), vertices)
  unlist(lapply(mget(vertices, envir = x), neighbors_out.datagraph_vertex, names = names), use.names = useNames)
}

#' @param x a datagraph object
#'
#' @param vertices see neighbors_in and neighbors_out methods
#' @param mode character; one of "in"/"out"/"all"
#'
#' @rdname neighborhood
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


#' @param mode character; "all", "in" our "out
#' @param order integer; how far from the node to look
#' @rdname neighborhood
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
