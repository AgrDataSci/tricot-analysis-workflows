library(gosset)
library(PlackettLuce)
library(network)
library(GGally)
library(ggnet2)
library(sna) #for function  %v% 

#First fun data loading and preparation lines in pltree_mode


musa_ranks <- as.rankings(musa_trials_data)

musa_trials_adj <- adjacency(musa_ranks)

pl_conn <- PlackettLuce::connectivity(musa_trials_adj)

pl_conn

net <- network::network(musa_trials_adj)

types <- read.csv("data/processed/genotypes_info.csv")

types <- janitor::clean_names(types)

landrace <- types[types$biological_a_status  == "Landrace", ]$i_name

improved <- types[types$biological_a_status  == "Hybrid", ]$i_name

somaclone <- types[types$biological_a_status  == "Somaclonal variant", ]$i_name

genotypes <- network::network.vertex.names(net)

genotypes

net %v% "Type" = ifelse(genotypes %in% somaclone, "Somaclone",
                        ifelse(genotypes %in% landrace, "Landrace", "Hybrid"))

type_palette_1 <- c("Landrace" = "#FF7F00",
                    "Hybrid" = "#99CC33",
                    "Somaclone" = "#4575b4")

#                    "Released Variety" = "#99CC33")

# type_palette_2 <- c(#"Landrace" = "#f1c40f",
#   "Experimental line" = "#e74c3c",
#   "Released Variety" = "#52be80")


GGally::ggnet2(net, 
               label = TRUE,
               arrow.size = 8, 
               edge.color = "darkgrey",
               mode = "kamadakawai",
               arrow.gap = 0.03, 
               color = "Type",
               palette = type_palette_1,
               legend.size = 12) 

ggsave("output/figures/musa_trials_network.png", dpi = 300, width = 16, height = 12)



