
library("tidyverse")
library("magrittr")
library("PlackettLuce")
library("gosset")
library("BradleyTerryScalable")
library("igraph")

# farmer rankings
df <- "data/durumwheat.csv"
df %<>% 
  read_csv()

hh <- read_csv("data/hhsurvey_tricot.csv")
names(hh)
hh %<>% 
  filter(Discrepancies == 1,
         !is.na(hhid))

keep <- df$id %in% hh$id

df <- df[keep,]

df %<>% 
  filter(region != "Oromia")


table(df$accession, df$region)

R <- to_rankings(data = df,
                 items = "genotype",
                 input = "farmer_rank",
                 id = "id")


adj <- adjacency(R)

connectivity(adj)

adj <- as.vector(adj)

adj <- t(matrix(adj, nrow = ncol(R), ncol = ncol(R)))

dimnames(adj) <- list(dimnames(R)[[2]], dimnames(R)[[2]])

adj <- btdata(adj, return_graph = TRUE)

plot.igraph(adj$graph, vertex.size = 5, edge.arrow.size = 0.1) 




