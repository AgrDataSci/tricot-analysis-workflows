worth_map2 = function(object, 
                      labels, 
                      labels.order = NULL, 
                      items.order = NULL, 
                      drop.items = NULL, ...) {
  
  if (is.null(labels.order)) {
    lvls = labels
  } else {
    lvls = labels.order
  }
  
  winprobs = gosset:::.combine_coeffs(object)
  names(winprobs) = labels
  
  if (is.null(items.order)) {
    order_items = order(exp(winprobs[,lvls[[1]]]))
    order_items = rownames(winprobs)[order_items]
  } else {
    order_items = items.order
  }
  
  winprobs = data.frame(items = rep(dimnames(winprobs)[[1]], times = ncol(winprobs)),
                        labels = rep(dimnames(winprobs)[[2]], each = nrow(winprobs)),
                        winprob = as.numeric(unlist(winprobs)))
  
  winprobs$labels = factor(winprobs$labels, levels = rev(lvls))
  
  if (!is.null(drop.items)) {
    keep = !winprobs$items %in% drop.items
    winprobs = winprobs[keep, ]
  }
  
  winprobs$items = factor(winprobs$items, levels = rev(order_items))
  
  items  = winprobs$items
  winprob = winprobs$winprob
  lims = max(abs(winprob)) * c(-1, 1)
  
  angle = 0
  if (any(nchar(labels) > 30)) angle = 40
  
  # === per-item axis label colors ===
  label_colors = c('Baba 70' = "red",
                   'Game Changer' = "red",
                   'IITA-TMS-IBA30572' = "blue", 
                   'IITA-TMS-IBA980581' = "blue", 
                   'K195' = "black", 
                   'Obasanjo 2' = "red",  
                   'TMEB2'  = "darkgreen", 
                   'TMEB419' = "blue",
                   'TMEB693'  = "black", 
                   'TMEB7' = "darkgreen", 
                   'TMS-13F0153P0001' = "black", 
                   'TMS-13F1049P0001' = "black",
                   'TMS-13F1088P0007' = "black", 
                   'TMS-13F1153P0001' = "black", 
                   'TMS-13F1307P0016' = "black", 
                   'TMS-13F1343P0002' = "black", 
                   'TMS-13F1343P0044' = "black", 
                   'TMS-13F2110P0008' = "black", 
                   'TMS-14F1035P0004' = "black", 
                   'TMS-14F1195P0005' = "black", 
                   'TMS-14F1223P0007' = "black", 
                   'TMS-14F1287P0008' = "black", 
                   'TMS-14F1291P0003' = "black", 
                   'TMS-14F1291P0012' = "black",
                   'TMS-14F1301P0004' = "black",
                   'TMS-14F1301P0013' = "black", 
                   'TMS-14F1306P0015' = "black", 
                   'TMS-14F1310P0004' = "black", 
                   'TMS-14F1310P0015' = "black", 
                   'TMS-IBA961632' = "blue")
  
  # Build HTML-colored labels for current x levels
  x_lvls <- levels(winprobs$items)
  x_cols <- unname(label_colors[x_lvls])
  x_cols[is.na(x_cols)] <- "grey20"  # default color
  colored_x_labels <- setNames(
    sprintf("<span style='color:%s'>%s</span>", x_cols, x_lvls),
    x_lvls
  )
  
  p = ggplot2::ggplot(winprobs, 
                      ggplot2::aes(x = items, 
                                   y = labels,
                                   fill = winprob,
                                   label = as.character(round(winprob, 2)))) +
    ggplot2::geom_tile() + 
    ggplot2::scale_fill_distiller(palette = "BrBG", 
                                  limit = lims,
                                  direction = 1,
                                  na.value = "white",
                                  name = "") +
    # apply colored labels
    ggplot2::scale_x_discrete(labels = colored_x_labels) +
    ggplot2::theme_minimal() +
    {
      # Use element_markdown() if ggtext is available, else fall back to plain text
      if (requireNamespace("ggtext", quietly = TRUE)) {
        ggplot2::theme(
          axis.text = ggplot2::element_text(color = "grey20"),
          strip.text.x = ggplot2::element_text(color = "grey20"),
          axis.text.x = ggtext::element_markdown(angle = 40, vjust = 1, hjust = 1, face = "bold"),
          axis.text.y = ggplot2::element_text(angle = angle, vjust = 1, hjust = 1),
          panel.grid = ggplot2::element_blank()
        )
      } else {
        ggplot2::theme(
          axis.text = ggplot2::element_text(color = "grey20"),
          strip.text.x = ggplot2::element_text(color = "grey20"),
          axis.text.x = ggplot2::element_text(angle = 40, vjust = 1, hjust = 1, face = "bold"),
          axis.text.y = ggplot2::element_text(angle = angle, vjust = 1, hjust = 1),
          panel.grid = ggplot2::element_blank()
        )
      }
    } +
    ggplot2::labs(x = "", y = "")
  
  return(p)
}


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
  x = gsub("Finalevaluationoverallimpression", "Overall", x)
  x = gsub("Overallimpression", "Overall", x)
  x = gsub("overallimpression", "Overall", x)
  x = gsub("Storage", "Storage ", x)
  x = gsub("Preparation", "Preparation ", x)
  x = gsub("resistance", "Resistance", x)
  x = gsub("tolerance", "Tolerance", x)
  x = gsub("colour", "Colour", x)
  x = gsub("yield", " Yield", x)
  x = gsub("content", "Content", x)
  x = gsub("Weedcompetitivenesscanopyformation", "Weed Competitiveness", x)
  x = gsub("Survivalsproutingnrofplantsleft", "Survival Sprouting", x)
  x = gsub("Survivalsprouting", "Survival Sprouting", x)
  x = gsub("Branchinghabitarchitectureoftheplant", "Branching Habit", x)
  x = gsub("ebaqualitymouldability", "Mouldability", x)
  x = gsub("notstickinesstohand Mouth", "Not Stickiness", x)
  x = gsub("swellingofgari", "Swelling", x)
  x = gsub("notstickiness", "Not Stickiness", x)
  x = gsub("Woodyfilament", "Woody Filament", x)
  x = gsub("Gariqualitydryness", "Gari Dryness", x)
  x = gsub("Waterreleaseduringpressing", "Water Release", x)
  x = gsub("Mashpulpdiscolor", "MashPulpDiscolor", x)
  x = gsub("Gariprocessingeaseofpeeling", "Easy Peeling", x)
  x = gsub("Suitabilitytosoilandenvironment", "Suitability to Soil", x)
  x = gsub("Germinationsprouting", "Sprouting", x)
  x = gsub("DensitywaterContent", "Water Content", x)
  x = gsub("Swellingofmash", "Swelling Mash", x)
  x = gsub("Choiceforreplant", "Choice for Replant", x)
  x = gsub("Plantgrowth", "Plant Growth", x)
  x = gsub("DiseaseResistance", "Disease Resistance", x)
  x = gsub("GariYield", "Gari Yield", x)
  x = gsub("Rootsize", "Root Size", x)
  x = gsub("RootColour", "Root color", x)
  x = gsub("Rootshape", "Root shape", x)
  x = gsub("MashPulpDiscolor", "Mash Pulp Discolor", x)
  x = gsub("Freshroots", "Fresh roots", x)
  x = gsub("RootsYield", "Roots Yield", x)
  x = gsub("Rootrot", "Root rot", x)
  x = ClimMobTools:::.title_case(x)
  x = gsub("  ", " ", x)
  x = gsub("Harvesting Fresh Roots ", 
           "Harvesting ", x)
  x = gsub("Harvesting Harvesting Yield", "Harvesting Fresh Roots Yield", x)
  
  x = gsub("First Month ", "First Month - ", x)
  x = gsub("Third Month ", "Third Month - ", x)
  x = gsub("Sixth Month ", "Sixth Month - ", x)
  x = gsub("Ninth Month ", "Ninth Month - ", x)
  x = gsub("Harvesting ", "Harvesting - ", x)
  x = gsub("Gari Processing ", "Gari Processing - ", x)
  x = gsub("Gari Quality ", "Gari Quality - ", x)
  x = gsub("Eba Quality After Preparation ", "Eba Quality After Preparation - ", x)
  x = gsub("Eba Quality After Storage ", "Eba Quality After Storage - ", x)
  x = gsub("Final Evaluation ", "Final Evaluation - ", x)
  
  x
}


