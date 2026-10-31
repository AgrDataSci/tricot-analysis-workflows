# .......................................
# .......................................
# PlackettLuce tree with NextGen cassava data 
library("PlackettLuce")
library("tidyverse")
library("ClimMobTools")
library("caret")
library("gosset")
library("ggplot2")
library("patchwork")
library("ggparty")
library("mapview")
library("leaflet")
library("patchwork")
library("magrittr")
library("tidyverse")
library("reshape2")
library("heatmap3")
library("corpcor")
library("GeneNet")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")


source("script/helper-01-function.R")

capture.output(sessioninfo::session_info(),
               file = "script/sessioninfo/03_analysis_pltree.txt")

dir.create("output", 
           recursive = TRUE,
           showWarnings = FALSE)
# .......................................
# .......................................
# Organize the traits by data collection moment ####
# read the file 
list.files("data")
dat = read.csv("data/tricot-data-nextgen-cassava.csv")

# get the list of traits
trait_list = getTraitList(dat, pattern = c("_best", "_worst"))

traits = lapply(trait_list, function(x){
  x$trait_label
})

traits = unlist(traits)

traitlabels = fix_trait_labels(traits)

trait_list = getTraitList(dat,
                          pattern = c("_best", "_worst"),
                          trait.labels = traitlabels)

#.....................................
#.....................................
# Kendall partial cor ####
# perform this analysis first by doing a backward selection
# removing the least correlated traits then, using the 
# selected traits run splitting by men and women
# an index for the packages 
pack_index = paste0("variety", letters[1:3])

itemnames = sort(unique(unlist(dat[, pack_index])))

R = lapply(trait_list, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = TRUE)
})

traits

# check data availability
cbind(traits, unlist(lapply(trait_list, function(x) sum(x$keep))))

# ....................................
### Backward selection to get the significant traits with p < 0.05 #######
true = TRUE
traits_to_keep = traitlabels
reference = which(grepl("Choice For Replant", traits_to_keep))
reference
pval = 0.05
R_to_backward = R
iteration = 1


kendall = lapply(R[-reference], function(x){
  kendallTau(x, R[[reference]])
})

kendall = do.call("rbind", kendall)

kendall$trait = traitlabels[-reference]

while(true) {

  cat("Iteration num.", iteration, "\n")

  rN = kendallTauCorMatrix(R_to_backward)

  r = rN[[1]]

  N = rN[[2]]

  # Test with the full matrix, just to see if it works
  pr = cor2pcor(r)

  # Simple way to work with these values is to convert to Pearson's r
  r_pearson = sin(3.141592654 * r * .5)

  dimnames(r_pearson) = list(traits_to_keep, traits_to_keep)

  #calculate pearson correlation P-value
  ttv = list()
  ttest_pvalue_matrix = matrix(NA, nrow = length(traits_to_keep), ncol = length(traits_to_keep))

  # Fill the matrix
  for(i in seq_along(traits_to_keep)){
    for (j in seq_along(traits_to_keep)){ #Fills lower triangle only, to avoid calculating the same value twice
      keep_tt = r_pearson[j,i] *sqrt((mean(N) - 2)/ (1-(r_pearson[j,i]^2)))
      ttv = keep_tt
      keep_tt_value = 2 * (pt(-abs(ttv), mean(N) - 2))
      ttest_pvalue_matrix[j,i] = keep_tt_value
    }
  }

  # get pvalues compared to the reference trait
  pvalues = ttest_pvalue_matrix[, reference]

  # check if any pvalue is higher than the threshold
  true = any(pvalues > pval)

  if (true) {
    # remove the trait with higher pvalue
    max_pvalue = which.max(pvalues)

    traits_to_keep = traits_to_keep[-max_pvalue]

    R_to_backward = R_to_backward[-max_pvalue]

    reference = which(grepl("Choice For Replant", traits_to_keep))

    iteration = iteration + 1
  }
}

cor_backward = data.frame(trait = traits_to_keep,
                          cor = r_pearson[, reference],
                          pval = ttest_pvalue_matrix[, reference])


cor_backward = separate(cor_backward,
                        "trait",
                        c("cropstage", "croptrait"),
                        sep = " - ",
                        remove = FALSE)

cor_backward = cor_backward[cor_backward$cor > 0, ]

drop = grepl("Choice For Replant", cor_backward$trait)

cor_backward[drop, "cor"] = NA

cor_backward = na.omit(cor_backward)

write.csv(cor_backward,
          "output/kendall-partial-correlation-backward-selection.csv",
          row.names = FALSE)

# cor_backward = read.csv("output/kendall-partial-correlation-backward-selection.csv")

# retain only the traits with correlation higher than 0.3
cor_backward = cor_backward[cor_backward$cor >= 0.3, ]

cor_backward$cropstage = factor(cor_backward$cropstage, 
                                levels = rev(unique(cor_backward$cropstage)))
cor_backward$cropstage

cor_backward = cor_backward[order(cor_backward$cor), ]

cor_backward$trait = factor(cor_backward$trait, 
                            levels = unique(cor_backward$trait))

head(cor_backward)

# make a bar plot plot
cor_plot =
  ggplot(data = cor_backward,
         aes(y = cor,
             x = croptrait,
             fill  = cropstage)) +
  geom_chicklet(show.legend = TRUE) +
  coord_flip() +
  facet_grid(rows = vars(cropstage),
             scales = "free",
             space = "free") +
  scale_fill_manual(values = c('#a1d99b','#74c476','#41ab5d',
                               '#238b45','#006d2c','#00441b')) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        strip.background =element_rect(fill="white"),
        text = element_text(color = "grey10"),
        strip.text.y = element_blank(),
        legend.position = "right",
        strip.text = element_text(size = 16, color = "grey10"),
        legend.text = element_text(size = 16, color = "grey10"),
        axis.text = element_text(size = 16, color = "grey10"),
        axis.title = element_text(size = 16, color = "grey10"),
        legend.title = element_blank()) +
  labs(x = "",
       y = "Kendal rank correlation with Choice for Replanting")

cor_plot

ggsave("output/kendall-tau-partial-correlation-all.pdf",
       plot = cor_plot,
       height = 10,
       width = 12)

# ..................................
# ..................................
# Split by gender ####
# partial correlation with gender using traits selected in backward process
gender = unique(na.omit(dat$sex))

trait_keep = traitlabels %in% union(cor_backward$trait, "Final Evaluation - Choice For Replant")

trait_sel = traitlabels[trait_keep]

R_sel = R[trait_keep]

trait_sel_string = gsub(" ", "", trait_sel)

reference_trait = which(grepl("Choice For Replant", trait_sel))
 
trait_cor = data.frame()

for (g in seq_along(gender)) {

  Rg = lapply(R_sel, function(x){
    x[dat$sex == gender[g],]
  })

  rN = kendallTauCorMatrix(Rg)

  r = rN[[1]]

  N = rN[[2]]

  # Test with the full matrix, just to see if it works
  pr = cor2pcor(r)

  # Simple way to work with these values is to convert to Pearson's r
  r_pearson = sin(3.141592654 * r * .5)

  dimnames(r_pearson) = list(trait_sel_string, trait_sel_string)

  #calculate pearson correlation P-value
  ttv = list()
  ttest_pvalue_matrix = matrix(NA,
                               nrow = length(trait_sel_string),
                               ncol = length(trait_sel_string))

  #Fill the matrix
  for(i in seq_along(trait_sel_string)){
    #Fills lower triangle only, to avoid calculating the same value twice
    for (j in seq_along(trait_sel_string)){
      keep_tt = r_pearson[j,i] *sqrt((mean(N) - 2)/ (1-(r_pearson[j,i]^2)))
      ttv = keep_tt
      keep_tt_value = 2 * (pt(-abs(ttv), mean(N) - 2))
      ttest_pvalue_matrix[j,i] = keep_tt_value
    }
  }

  cor_g = data.frame(trait= trait_sel,
                     cor = r_pearson[, reference_trait],
                     pval = ttest_pvalue_matrix[, reference_trait],
                     gender = gender[g])

  trait_cor = rbind(trait_cor, cor_g)

}

trait_cor = separate(trait_cor,
                     "trait",
                     c("cropstage", "croptrait"),
                     sep = " - ",
                     remove = FALSE)

write.csv(trait_cor, "output/kendall-cor-from-rankings-gender-split.csv",
          row.names = FALSE)

# trait_cor = read.csv("output/kendall-cor-from-rankings-gender-split.csv")

# get the traits with p < 0.05
keep = trait_cor$pval < 0.05

keep = unique(trait_cor$trait[keep])

keep = trait_cor$trait %in% keep

trait_cor = trait_cor[keep, ]

drop = grepl("Choice For Replant", trait_cor$trait)

trait_cor[drop, "cor"] = NA

trait_cor = na.omit(trait_cor)

# force order of labels using the first wrap
trait_cor$cropstage = factor(trait_cor$cropstage, 
                                levels = rev(unique(trait_cor$cropstage)))
trait_cor$cropstage

ord = order(trait_cor[trait_cor$gender == "Man", "cor"])

lev = trait_cor[trait_cor$gender == "Man", "trait"][ord]

trait_cor$trait = factor(trait_cor$trait, levels = lev)

# add N by gender
ngender = table(na.omit(dat$sex))

trait_cor$gender = ifelse(trait_cor$gender == "Man",
                          paste0(trait_cor$gender, " (n = ", ngender[1], ")"),
                          ifelse(trait_cor$gender == "Woman",
                                 paste0(trait_cor$gender, " (n = ", ngender[2], ")"),
                                 trait_cor$gender))

head(trait_cor)
# make a bar plot plot
cor_plot =
  ggplot(data = trait_cor,
         aes(y = cor,
             x = croptrait,
             fill = cropstage)) +
  geom_chicklet(show.legend = TRUE) +
  coord_flip() +
  facet_grid(rows = vars(cropstage),
             cols = vars(gender),
             scales = "free",
             space = "free") +
  scale_fill_manual(values = c('#a1d99b','#74c476','#41ab5d',
                               '#238b45','#006d2c','#00441b')) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        strip.background =element_rect(fill="white"),
        text = element_text(color = "grey10"),
        strip.text.y = element_blank(),
        strip.background.x = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 16, color = "grey10"),
        legend.text = element_text(size = 16, color = "grey10"),
        axis.text = element_text(size = 16, color = "grey10"),
        axis.title = element_text(size = 16, color = "grey10"),
        legend.title = element_blank()) +
  labs(x = "",
       y = "Kendal rank correlation with Choice for Replanting")

cor_plot

ggsave("output/kendall-tau-partial-correlation-gender-split.pdf",
       plot = cor_plot,
       height = 10,
       width = 12)

