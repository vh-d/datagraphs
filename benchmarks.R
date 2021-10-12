
N <- 1e4

igraph_time1 <-
  system.time({
    ig <- igraph::make_empty_graph()
    ig <- igraph::add_vertices(ig, nv = 1, name = as.character(1L))
    for (i in 2:N) {
      ig <- igraph::add_vertices(ig, nv = 1, name = as.character(i))
      ig <- igraph::add_edges(ig, c(i-1, i), name = as.character(i))
    }
  })

dg_time1 <-
  system.time({
    dg <- datagraph()
    datagraphs::add_vertex(dg, vertex = list(id = as.character(1L)))
    for (i in 2:N) {
      datagraphs::add_vertex(list(id = as.character(i)), graph = dg)
      datagraphs::add_edge(list(from = as.character(i-1L), to = as.character(i)), graph = dg)
    }
  })

print(igraph_time1)
print(dg_time1)
