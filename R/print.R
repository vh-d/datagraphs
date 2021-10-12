

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
