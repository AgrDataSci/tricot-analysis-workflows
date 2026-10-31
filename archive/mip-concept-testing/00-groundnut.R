# .......................................
# .......................................
# This script runs the PlackettLuce model 
# to assess farmers preferences 
# Load packages
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
library("multcompView")
library("ggplot2")
library("ggparty")
library("patchwork")
library("readxl")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
# source("/Users/kauedesousa/Library/Mobile Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/modules/01_functions.R")

dir.create("output/groundnut/", showWarnings = FALSE, recursive = TRUE)

# read the data  
# this is a csv file previously treated from the original 
# .json file with the farmers' responses on the preferred concepts
dat = read_excel("data/groundnut/Gnut-TZ-Phase1- Concept Testing-maintable.xlsx")

names(dat)

dat = dat[!is.na(dat$resp_gender), ]

table(dat$resp_gender)

# put responses back to A, B, C
dat$variety_preferred

dat$best = apply(dat[, c(paste0("video", 1:3), "variety_preferred")], 1, function(x){
  LETTERS[which(as.vector(x[4]) == as.vector(unlist(x[1:3])))]
})

dat$variety_lesspreferred

dat$worst = apply(dat[, c(paste0("video", 1:3), "variety_lesspreferred")], 1, function(x){
  LETTERS[which(as.vector(x[4]) == as.vector(unlist(x[1:3])))]
})

sum(dat$best == dat$worst)

# put concept names as title case
head(dat[paste0("video", 1:3)])

dat[paste0("video", 1:3)] = lapply(dat[paste0("video", 1:3)], ClimMobTools:::.title_case)

sort(unique(unlist(dat[paste0("video", 1:3)])))

dat[paste0("video", 1:3)] = lapply(dat[paste0("video", 1:3)], function(x){
  x = gsub("Early Maturity Variety", "Drought Escape", x)
  x = gsub(" Variety", "", x)
  x = gsub(" variety", "", x)
  x
})

table(unlist(dat[paste0("video", 1:3)]))

# start data analysis by making the grouped rankings
names(dat)

dat$variety_lesspreferred
dat$variety_preferred
# .........................................
# .........................................
# PlackettLuce model ####
G = rank_tricot(dat,
                paste0("video", 1:3),
                c("best", "worst"),
                group = TRUE)


mod = PlackettLuce(G)

mod

lvls = c('Drought Resistant','Intercropping','High Nutritional','Drought Escape',
         'Home Cooking','Snacking', 'Dual-purpose', 'Low Labor',  'High Oil', 
         'Industrial Processor')

ref = 'Drought Escape'

logworth_all = 
  plot_logworth(mod, ref = ref, levels = rev(lvls)) + 
  labs(y = "Conceptual variety",
       x = "Log-worth") + 
  theme(axis.title = element_text(size = 13),
        axis.text.x = element_text(angle = 0),
        strip.text = element_blank()) 

logworth_all

ggsave("output/groundnut/log-worth-all.png",
       plot = logworth_all,
       width = 15,
       height = 10,
       units = "cm",
       dpi = 600)

ggsave("output/groundnut/log-worth-all.pdf",
       plot = logworth_all,
       width = 15,
       height = 10,
       units = "cm",
       dpi = 600)

# calculate reliability
rel = reliability(mod, ref = ref)

rel

write.csv(rel, file = "output/groundnut/reliability-all.csv", row.names = FALSE)


table(dat$districtid)

# now try to split by country and gender
pld = data.frame(G, 
                 Gender = as.factor(dat$resp_gender), 
                 District = as.factor(dat$districtid))

head(pld)

str(pld)

# fit the PlackettLuce tree
tree1 = pltree(G ~ District + Gender, 
               data = pld,
               gamma = TRUE, 
               alpha = 0.05)

tree1

tree2 = pltree(G ~ Gender, 
               data = pld,
               gamma = TRUE, 
               alpha = 0.05)

tree2

deviance(tree1)

deviance(tree2)


ptree = plot(tree1,
             ref = ref, 
             multcomp = TRUE,
             levels = lvls)

ptree

ggsave("output/groundnut/pltree-log-worth.png",
       plot = ptree,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)

ggsave("output/groundnut/pltree-log-worth.pdf",
       plot = ptree,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)

# now the tree showing the worth
ptree2 = plot(tree1, log = FALSE, multcomp = TRUE)

ptree2

ggsave("output/groundnut/pltree-log-worth-winprobability.png",
       plot = ptree2,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)

ggsave("output/groundnut/pltree-worth-winprobability.pdf",
       plot = ptree2,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)

# compute reliability for the tree 
regret = regret(tree1, n1 = 1000)

regret

write.csv(regret, 
          "output/groundnut/regret-risk-assessment.csv",
          row.names = FALSE)


# .........................................
# .........................................
# get pairwise comps matrix using worth ####
pair_worth = matrix(0, 
                    nrow = length(lvls), 
                    ncol = length(lvls),
                    dimnames = list(rev(lvls), rev(lvls)))

worths = coef(mod, log = FALSE)
sort(worths)

for(i in seq_len(ncol(pair_worth))) {
  
  for(j in seq_len(nrow(pair_worth))) {
    
    col_i = dimnames(pair_worth)[[1]][i]
    row_j = dimnames(pair_worth)[[2]][j]
    
    if(col_i == row_j) next
    
    print(c(col_i, row_j))
    
    w = worths[col_i] / (worths[col_i] + worths[row_j])
    
    w = round(w, 2)
    
    pair_worth[row_j, col_i] = w
    
  }
  
}

pair_worth

write.csv(t(pair_worth), 
          file = "output/groundnut/pairwise-probabilities.csv", 
          row.names = TRUE)

pair_worth = pair_worth - 0.5
pair_worth[pair_worth == -0.5] = 0

write.csv(t(pair_worth), 
          file = "output/groundnut/pairwise-probabilities-with-losses.csv",
          row.names = TRUE)


pair_dat = data.frame(player1 = rep(rev(lvls), each = length(lvls)), 
                      player2 = rep(rev(lvls), times = length(lvls)),
                      worth = as.vector(pair_worth))

pair_dat$player1 = factor(pair_dat$player1, levels = lvls)

pair_dat$player2 = factor(pair_dat$player2, levels = rev(lvls))

#pair_dat$worth[pair_dat$worth == 0] = NA

pair_dat$worth = round(pair_dat$worth, 2)

p = ggplot(pair_dat, 
           aes(x = player2, 
               y = player1,
               fill = worth,
               label = worth)) +
  geom_tile() + 
  geom_text() +
  scale_fill_gradient2(low = "#b2182b", 
                       high = "#2166ac", 
                       mid = "white",
                       midpoint = 0) +
  scale_x_discrete(position = "top") +
  theme_bw() +
  theme(axis.text = element_text(color = "grey10"),
        strip.text.x = element_text(color = "grey10"),
        axis.text.x = element_text(angle = 90, hjust = 0),
        panel.grid = element_blank()) +
  labs(x = "", 
       y = "",
       fill = "")

p
ggsave("output/groundnut/pairwise-probabilities.pdf",
       plot = p,
       width = 20,
       height = 20,
       units = "cm",
       dpi = 600)





