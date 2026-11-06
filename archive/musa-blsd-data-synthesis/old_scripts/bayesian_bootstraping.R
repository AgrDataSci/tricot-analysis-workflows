#Bayesian bootstraping 


n <- nrow(musa_trials_data)

weights <-  rexp(n, 1)/n

weights <- weights / sum(weights) 

sum(weights)

sum(weights > 0)

hist(weights)

plt_models <- vector(mode = "list", length = 500)

for(i in seq_len(500)){
  
  weights <-  rexp(n, 1)/n
  
  #sum(weights > 0)
  
  #weights <- weights / sum(weights)
  
  
  plt_models[[i]] <- pltree(musa_ranks ~ .,
                            data = musa_data,
                            npseudo = 0.5,
                            alpha = .9,
                            bonferroni = TRUE,
                            verbose = TRUE,
                            ncores = 7,
                            dfsplit = FALSE,
                            prune = "AIC",
                            weights = weights,
                            caseweights = FALSE,
                            minsize = floor(nrow(musa_data)/3),
  )
}