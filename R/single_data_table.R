#'
#'
#' #' @export
#' as.datagraph.igraph <- function(x, ...) {
#'
#'   obj   <- igraph::as_data_frame(x, what = "vertices")
#'   edges <- igraph::as_data_frame(x, what = "edges")
#'
#'   if (!("id" %in% names(obj))) obj$id <- rownames(obj)
#'   setDT(obj, key = "id")
#'   setcolorder(obj, "id")
#'
#'   setDT(edges, key = c("from", "to"))
#'   edges[, `:=`(from = as.character(from), to = as.character(to))]
#'   obj[edges[, .(to   = list(to)),   by = .(id = from)], on = "id", to   := i.to]
#'   obj[edges[, .(from = list(from)), by = .(id = to)],   on = "id", from := i.from]
#'
#'   class(obj) <- c("datagraph", "data.table", "data.frame")
#'
#'   return(obj)
#' }
#'
#'
#' #' @export
#' as.datagraph.data.table <- function(x, vertices = NULL) {
#'   # validate inputs
#'   stopifnot(all(c("from", "to") %in% names(x)))
#'
#'   # edges
#'   edges <- setDT(copy(unique(x)), key = c("from", "to"))
#'
#'   if (!is.null(vertices)) {
#'     stopifnot("id" %in% colnames(vertices))
#'     obj <- setDT(copy(vertices))
#'   } else {
#'     obj <- edges[, .(id = union(from, to))]
#'   }
#'
#'   setkey(obj, id)
#'   obj[edges[, .(to   = list(to)),   by = .(id = from)], on = "id", to   := i.to]
#'   obj[edges[, .(from = list(from)), by = .(id = to)],   on = "id", from := i.from]
#'
#'   class(obj) <- c("datagraph", "data.table", "data.frame")
#'
#'   return(obj)
#' }
#'
#'
#' #' @importFrom igraph as.igraph
#' #' @export
#' as.igraph.datagraph <- function(x, ...) {
#'   igraph::graph_from_data_frame(
#'     d = x[, .(to = unlist(to)), by = .(from = id)],
#'     vertices = setnames(x[, !c("from", "to"), with = FALSE], "id", "name"),
#'     directed = TRUE
#'   )
#' }
#'
#'
#' #' @export
#' add_vertices.datagraph <- function(x, new, ...) {
#'   rbindlist(
#'     list(
#'       x,
#'       new
#'     ),
#'     use.names = TRUE,
#'     fill = TRUE
#'   )
#' }
#'
#' #' @export
#' remove_vertices.datagraph <- function(x, ids, ...) {
#'   obj <- x[!(id %in% ids)]
#'   obj[, from := lapply(from, setdiff, ids)]
#'   obj[, to   := lapply(to,   setdiff, ids)]
#'
#'   obj[]
#' }
#'
#'
#' #' @export
#' add_edges.datagraph <- function(graph, edges) {
#'   graph[.(id = edges[["from"]]), on = "id", nomatch = 0, to   := mapply(union, to,   edges[["to"]])]
#'   graph[.(id = edges[["to"]]),   on = "id", nomatch = 0, from := mapply(union, from, edges[["from"]])]
#'
#'   graph[]
#' }
#'
#'
#' #' @export
#' remove_edges.datagraph <- function(graph, edges) {
#'   graph[.(id = edges[["from"]]), on = "id", nomatch = 0, to   := mapply(setdiff, to,   edges[["to"]])]
#'   graph[.(id = edges[["to"]]),   on = "id", nomatch = 0, from := mapply(setdiff, from, edges[["from"]])]
#'
#'   graph[]
#' }
#'
#' #' @export
#' detect_cycles.datagraph <- function(x, ...) {
#'   x <- copy(x)
#'   x[, keep := TRUE]
#'   continue <- 2 # TODO: or 1?
#'   while (continue > 0) {
#'     nx <- x[(keep), .N]
#'     d <- x[!(sapply(from, length)>0 & sapply(to, length)>0), id]
#'     x[(id %in% d), keep := FALSE] # flag
#'     x[(keep), from := lapply(from, setdiff, d)]
#'     x[(keep), to   := lapply(to,   setdiff, d)]
#'     if (x[(keep), .N] == nx) continue <- continue - 1
#'   }
#'   x <- x[(keep), !"keep"]
#'   setkey(x, id)
#'   return(x[])
#' }
#'
#' #' @export
#' print.datagraph <- function(x, ...) {
#'   cat("<datagraph>\n")
#'   NextMethod(x, ...)
#' }
#'
#'
#' #' @export
#' check.datagraph <- function(x, ...) {
#'   checks <-
#'     c(
#'       required_cols = all(c("id", "from", "to") %in% names(x)),
#'       symmetry = identical(
#'         x[, .(from = unlist(from)),  by = .(to = id)][,   .(from , to)] |> setkey(from, to),
#'         x[, .(to   = unlist(to)), keyby = .(from = id)] |> setkey(from, to)
#'       )
#'     )
#'
#'   if (all(checks)) {
#'     return(TRUE)
#'   }else {
#'     return(
#'       structure(.Data = FALSE, checks = checks)
#'     )
#'   }
#' }
