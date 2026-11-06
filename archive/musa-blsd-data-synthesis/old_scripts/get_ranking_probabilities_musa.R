

#testing_objects
# 
# .plt_model <- plt_01
# .newdata <- primera_newdata
# .nsim <- 1000
# .top_k <- 3

#get rank probabilities of genotypes from bootstrapped estimates
get_ranking_probabilities <- function(.plt_model, .newdata, .nsim = 1000, .top_k){
  
  #get rank probabilities of fitted model
  mod_rank_probs <- get_rankProb(.plt_model, n = .nsim)
  
  #get the ranking probabilities of top-k genotypes
  top_k_probs <- lapply(mod_rank_probs, function(X){
    rbind(colSums(X[1:.top_k, ]),
          colSums(X[(.top_k + 1):dim(X)[1], ]))
  }
    )
  
  #select only the probabilities of those in the top-k
  top_k_probs_upper <- lapply(top_k_probs, function(X)X[1, ])
  
  
  #predict in which node of the plt each cell falls
  pred_nodes <- prim_nodes <- lapply(.newdata,
                                     function(X) predict(.plt_model, 
                                                         newdata = X, 
                                                         type = "node"))
  pred_nodes_df <- as.data.frame(pred_nodes)
  
  colnames(pred_nodes_df) <- paste("y", seq_along(.newdata), sep = "_")
  
  #get average probability of rank probabilities in years in newdata
  avg_rank_prob <- calc_cell_rank_prob(pred_nodes_df, top_k_probs_upper, .top_k)
  
  #get entropies of top_k genotypes
  mod_entr <- get_entropies(top_k_probs)
  
  #get average entropy of years in newdata
  avg_entr <- calc_cell_entr(pred_nodes_df, mod_entr)
  
  #get entropy by each predicted genotype
  entropy <- get_entropy_gen(avg_entr, avg_rank_prob$gen_names_list)
  
  avg_rank_prob$entropy <- entropy
  
  return(avg_rank_prob)
  
}

# .nodes_df <- pred_nodes_df
# .top_k_rank_probs <- top_k_probs_upper
# .top_k <- 3
# 
# length(.mod_rank_probs)
# mod_rank_probs[unique(unlist(.nodes_df[i,]))]

#calculate average rank probabilities for the years in newdata
#.nodes_df data.frame with predicted nodes in which each cell fall
#.mod_rank_probs rank probabilities of each node in the fitted model
#.top_k number of genotypes in the rank. Suggested values 1 for the best or 3 for the top-3



calc_cell_rank_prob <- function(.nodes_df, .top_k_rank_probs, .top_k){
  
  cell_rank_prob_result <- vector(mode = "list", length = 3) 
  
  names(cell_rank_prob_result) <- c("rank_probs", "gen_names")
  
  nodes <- unique(unlist(.nodes_df))
  
  n_nodes <- length(.top_k_rank_probs)
  
  # .top_k_rank_probs <- vector(mode = "list", length = n_nodes)
  # 
  # names(.top_k_rank_probs) <- names(.mod_rank_probs)
  # 
  # if(.top_k > 1){
  #   message("Computing rank probabilities of top-k genotypes")
  #   for(i in 1:n_nodes){
  #     
  #     .top_k_rank_probs[[i]] <- .mod_rank_probs[[i]][1:.top_k, ]
  #     
  #     .top_k_rank_probs[[i]] <- colSums(.top_k_rank_probs[[i]])
  #     
  #   }
  # }else{
  #   message("Computing rank probabilities of best genotypes")
  #   for(i in 1:n_nodes){
  #     
  #     .top_k_rank_probs[[i]] <- .mod_rank_probs[[i]][1:.top_k, ]
  #     
  #   }
  #   
  # }
  
  freq_nodes_cell <- matrix(0, nrow = nrow(.nodes_df), ncol = length(nodes))
  
  colnames(freq_nodes_cell) <- nodes
  
  nyears <- ncol(.nodes_df)
  
  ngen <- length(.top_k_rank_probs[[1]])
  
  gen_names <- names(.top_k_rank_probs[[1]])
  
  avg_rank_probs_yrs <- matrix(nrow = nrow(.nodes_df), ncol = ngen)
  
  colnames(avg_rank_probs_yrs) <- gen_names
  
  top_k_names_cell_pasted <- vector(mode = "character", length = nrow(freq_nodes_cell))
  
  top_k_names_cell <- vector(mode = "list", length = nrow(freq_nodes_cell))
  
  top_k_prob <- vector(mode = "numeric", length = nrow(freq_nodes_cell))
  
  for(i in 1:nrow(freq_nodes_cell)){
    
    rank_prob_nodes <- unlist(.top_k_rank_probs[unique(unlist(.nodes_df[i,]))]) * rep(table(t(.nodes_df[i, ])) / nyears, each = ngen)
    
    names(rank_prob_nodes) <- gsub(pattern = "[0-9]\\.", replacement = "", x = names(rank_prob_nodes))
    
    avg_rank_probs_yrs[i, names(rank_prob_nodes)] <- tapply(X = rank_prob_nodes, INDEX = names(rank_prob_nodes), FUN = sum)
    
    top_k_prob[i] <- mean(sort(avg_rank_probs_yrs[i, ], decreasing = TRUE)[1:.top_k])
    
    top_k_names_cell_pasted[[i]] <- paste0(names(sort(avg_rank_probs_yrs[i, ], decreasing = TRUE)[1:.top_k]), collapse = " > ")
    
    top_k_names_cell[[i]] <- names(sort(avg_rank_probs_yrs[i, ], decreasing = TRUE)[1:.top_k])
    
  }

  cell_rank_prob_result$rank_probs <- top_k_prob
  
  cell_rank_prob_result$gen_names <- top_k_names_cell_pasted
  
  cell_rank_prob_result$gen_names_list <- top_k_names_cell
    
  return(cell_rank_prob_result)
  
}

