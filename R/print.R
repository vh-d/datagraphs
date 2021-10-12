

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
  cat("Edges from:", names(x[["from"]]), "\n")
  cat("Edges to:",   names(x[["to"]]), "\n")
  cat("Data:", ls(x[["data"]], sorted = FALSE))
}

#' @export
print.datagraph_edge <- function(x) {
  cat("<datagraph_edge>", x[["id"]], "\n")
  if (!is.null(x[["data"]])) cat("\tData:", ls(x[["data"]], sorted = FALSE))
}
