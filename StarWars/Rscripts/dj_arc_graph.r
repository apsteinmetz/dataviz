
djCorpus_subset<-tm_filter(djCorpus, FUN = function(x) meta(x)[["DJ"]] %in% c(similar_DJS,dj1))
# get document-term matrix
djdtm_subset = DocumentTermMatrix(djCorpus_subset) %>% removeSparseTerms(0.50)

dj_mat<-as.matrix(djdtm_subset)

adj_mat1 = dj_mat %*% t(dj_mat)
# set zeros in diagonal
diag(adj_mat1) = 0
# create graph from adjacency matrix
graph_artists1 = graph.adjacency(adj_mat1, mode="undirected", weighted=TRUE, diag=FALSE)
# get edgelist 1
edges1 = get.edgelist(graph_artists1)

# arc widths based on graph_artists1
w1 = E(graph_artists1)$weight
lwds = w1/20000

# arc-diagram
arcDiagram(edges1, lwd=lwds, cex=0.8, mar=c(7,1,4,1))
title("Selected DJ Associtations", cex.main=0.9)


