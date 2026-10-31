#Define new function to obtain a Kendall tau correlation matrix and effective N matrix
kendallTauCorMatrix = function(L){
  
  n_i = length(L)
  
  # Check if the list is longer than 3 and check if it contains rankings
  stopifnot(n_i>2, inherits(L[[1]], "rankings"), inherits(L[[2]], "rankings"))
  
  #Prepare the correlation matrix to be filled with values
  r = matrix(NA, nrow=n_i, ncol=n_i)
  rownames(r) = names(L)
  colnames(r) = names(L)
  
  #Prepare the effective N matrix
  N = matrix(NA, nrow=n_i, ncol=n_i)
  rownames(N) = names(L)
  colnames(N) = names(L)
  
  #Fill the matrix
  for(i in 1:n_i){
    for (j in 1:i){ #Fills lower triangle only, to avoid calculating the same value twice
      
      rNij = kendallTau(L[[i]], L[[j]]) #TODO deal with NA values
      r[i,j] = as.numeric(rNij[1])
      N[i,j] = as.numeric(rNij[2]) 
      
    }
    
  }
  
  # Make matrices symmetric    
  r[upper.tri(r)] = t(r)[upper.tri(r)]
  N[upper.tri(N)] = t(N)[upper.tri(N)]
  
  # Wrap result
  results = list(Kendall_tau_corr = r, Effective_N = N)
  
  return(results)
  
}

# function set to work only with this data set, since the process repeats in 
# different scripts
fix_trait_labels = function(x){
  
  x = gsub("_qst_|_|tricot", " ", x)
  x = ClimMobTools:::.title_case(x)
  x = ClimMobTools:::.title_case(x)
  x = gsub("Podyield", "Pod yield", x)
  x = gsub("Haulmyield", "Haulm yield", x)
  x = gsub("yield", "Yield", x)
  x = gsub("Grainsize", "Grain Size", x)
  x = gsub("Graincolour", "Grain Color", x)
  x = gsub("content", " Content", x)
  x = gsub("resistance", " Resistance", x)
  x = gsub("tolerance", " Tolerance", x)
  x = gsub("labour", " Labor", x)
  x = gsub("Physiologicalmaturity ", "", x)
  x = gsub("Agronomicperformance ", "", x)
  x = gsub("Reproductive Diseases Resistance", "Diseases Resistance [Reproductive]", x)
  x = gsub("Reproductive Vigor", "Vigor [Reproductive]", x)
  x = gsub(" Overall", "", x)
  x = gsub("Vegetative ", "", x)
  x = gsub("Reproductive ", "", x)

  x
}


