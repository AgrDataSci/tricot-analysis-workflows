library(stablelearner)


s <- stablelearner::subsampling(B = 1000, v = .8)

#samples <- s$sampler(nrow(musa_data))


b <- stablelearner::bootstrap(B = 500, v = 1)

samples <- b$sampler(nrow(musa_data))



str(samples)
dim(sub_samp)

ncol(sub_samp)

length(sub_samp)
#sub_samp[,1000]
 
# musa_data[sub_samp[,1], ]

musa_data[!which(!is.na(musa_data$musa_ranks)) %in% sub_samp[,1], ]

nrow(musa_data[samples[,i], ])
  
  
  i_deviance <- numeric(length = ncol(samples))
  
  i_r2 <- numeric(length = ncol(samples))
  
  i_kw <- numeric(length = ncol(samples))
  
  i_kw_2 <- numeric(length = ncol(samples))
  
  i_kt <- numeric(length = ncol(samples))
  
  #sub_samp_size <- nrow(samples)
  
  plt_models <- vector(mode = "list", length = ncol(samples))
  
  res <- vector(mode = "list", length = 5)
  
  for(i in seq_len(ncol(samples))){
    
    print(i)
    
    train_data <- musa_data[samples[,i], ]
    
    test_data <- musa_data#[!which(!is.na(musa_data$musa_ranks)) %in% sub_samp[,i], ]
    
    plt_model <- pltree(musa_ranks ~ .,
                        data = train_data,
                        npseudo = 1.5,
                        alpha = .05,
                        bonferroni = T,
                        verbose = FALSE,
                        ncores = 7,
                        dfsplit = FALSE,
                        #prune = "AIC",
                        # weights = weights,
                        # caseweights = TRUE,
                        minsize = floor(nrow(musa_data)/3)
    )
    
    plt_models[[i]] <- plt_model
    
    i_deviance[i] <- deviance(plt_model, newdata = test_data)
    
    pred_rank <-  predict(plt_model, newdata = test_data, type = "rank")
    
    obs_rank <- musa_ranks[which(!is.na(test_data$musa_ranks)), , as.grouped_rankings = FALSE]
    
    i_r2[i] <- gosset::pseudoR2(plt_model, newdata = test_data)[[5]]
    
    i_kt[i] <- gosset::kendallTau(x = obs_rank, y = pred_rank)[[1]]
     
    # i_kw[i] <- DescTools::KendallW(data.frame("pred" = pred_rank,
    #                                         "obs" = obs_rank),
    #                              correct = TRUE,
    #                              test = TRUE)$estimate
    
    
    i_kw[i] <- mean(unlist(lapply(X = seq_len(nrow(pred_rank)),
                                  FUN = function(X){
                                    #print(X)
                                    a <- obs_rank[X, ]
                                    b <- pred_rank[X,]
                                    #b[b==0] <-NA
                                    a <- a[a > 0]
                                    #print(a)
                                    b <- b[names(b) %in% names(a)]
                                    
                                    #print(b)
                                    
                                    DescTools::KendallW(data.frame("obs" = a,
                                                        "pred" = b),
                                             correct = TRUE,
                                             test = TRUE)$estimate}
                                  )
                           )
                    )
                                   
  }
  
  tot_deviance <- round(mean(i_deviance), 3)
  
  median(i_deviance)
  
  #mean Pseudo R2 
  mean_r2_a <- round(mean(i_r2), 3)
  
  mean_kw <- round(mean(i_kw), 3)
  
  mean_kw_2 <- round(mean(i_kw_2), 3)
  
  mean_kt <- round(mean(i_kt), 3)
  
  res[[1]] <- data.frame(tot_deviance,
                         mean_r2_a,
                         mean_kw, 
                         mean_kw_2,
                         mean_kt)
  
  # res[[2]] <- plt_models
  # 
  # res[[3]] <- i_r2
  # 
  # res[[4]] <- i_deviance
  # 
  # res[[5]] <- kw
  # 
  # res[[6]] <- kw_2
  
#   return(res)
#   
# }

dim(obs_rank)  

dim(pred_rank)  
  

