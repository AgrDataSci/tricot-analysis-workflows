#Plot adjacency network of ranked elements
library(igraph)



plot_network <- function(.network){
  l <- layout_with_fr(.network)  
  l <- norm_coords(l, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  plot(.network, 
       rescale = F, 
       layout = l*1.2,
       vertex.color = "mediumvioletred",
       vertex.size = 3,
       vertex.label.color = "midnightblue",
       edge.arrow.size = 0.3,
       vertex.label.dist = 1, 
       vertex.label.cex = 0.8, 
       edge.color = "seashell4", 
       vertex.label.font = 2,
       edge.arrow.width = 1)
  
}


library(GGally)
library(network)

plot_rank_net <- function(rankings){
  
  adj <- adjacency(rankings)
  
  net <- network::network(adj)
  
  GGally::ggnet2(net = net,
                 label = T,
                 arrow.size = 8, 
                 edge.color = "grey26",
                 mode = "kamadakawai",
                 arrow.gap = 0.03) 
  
}
