library(PlackettLuce)

genotypes_info <- read.csv("data/processed/genotypes_info.csv")

genotypes_info <- janitor::clean_names(genotypes_info)

unique(genotypes_info$status)

genotypes_info$status_2 <- ifelse(genotypes_info$status == "Wild", "Landrace",
                                  ifelse(genotypes_info$status == "Somaclonal variant",
                                         "Hybrid", genotypes_info$status))

genotypes_info$status_g1_ <- factor(genotypes_info$status_2, levels = c("Landrace", "Hybrid"))

genotypes_info$status_g2_ <- factor(genotypes_info$status,
                                  levels = c("Wild", "Landrace", "Hybrid","Somaclonal variant"))

unique(genotypes_info$blsd)

genotypes_info$resistance <- ifelse(genotypes_info$blsd == "PR", "R",
                                    ifelse(genotypes_info$blsd == "U", "S",genotypes_info$blsd ))

unique(genotypes_info$resistance)

# genotypes_info$resistance <- factor(genotypes_info$blsd,
#                                     levels = c("S", "PR", "R", "U"))

genotypes_info$resistance <- factor(genotypes_info$resistance,
                                    levels = c("S", "R"))


# genotypes_info$BP_ <- ifelse(genotypes_info$i_name == "Calcutta 4",
#                              "Wild",
#                              ifelse(is.na(genotypes_info$breeding_program),
#                                     "Landrace",
#                                     ifelse(genotypes_info$breeding_program == "NARO-IITA",
#                                                        "IITA", genotypes_info$breeding_program)))

genotypes_info$BP_ <- ifelse(is.na(genotypes_info$breeding_program),
                             "Landrace",
                             ifelse(genotypes_info$breeding_program == "NARO-IITA",
                                    "IITA", genotypes_info$breeding_program))

unique(genotypes_info$BP_)

genotypes_info$BP_ <- factor(x = genotypes_info$BP_,
                             levels = c(#"Wild",
                                        "Landrace",
                                        "IITA",
                                        "CARBAP",
                                        "EMBRAPA",
                                        "FHIA",
                                        "TBRI",
                                        #"NARO-IITA",
                                        "INIVIT"))

#--------------------------------

musa_plt_nodes <- predict(musa_plt_01, type = "node") 


musa_plt_nodes

#rho = 1 is the default value

pladmm_all <- pladmm(musa_trials_data, ~ i_name,  data = genotypes_info, rho = 1)
logLik(pladmm_all) * 0.1

base_2 <- pladmm(musa_trials_data[musa_plt_nodes == "2",], ~ i_name, data = genotypes_info, rho = 1)
logLik(base_2) * .1

base_3 <- pladmm(musa_trials_data[musa_plt_nodes == "3",], ~ i_name, data = genotypes_info, rho = 1)
logLik(base_3) * .1

#Status Landrace vs Improved (Hybrids)

#node 2
pladmm_node_2_s1 <- pladmm(musa_trials_data[musa_plt_nodes == "2", ],
                        ~ status_f,
                        data = genotypes_info,
                        rho = 82)

summary(pladmm_node_2_s1)

pladmm_node_2_s2 <- pladmm(musa_trials_data[musa_plt_nodes == "2", ],
                        ~  status_2, 
                        data = genotypes_info,
                        rho = 82)

summary(pladmm_node_2_s2)



#node 3

pladmm_node_3_s1 <- pladmm(musa_trials_data[musa_plt_nodes == "3", ],
                           ~ status_f,
                           data = genotypes_info,
                           rho = 82)

summary(pladmm_node_3_s1)

pladmm_node_3_s2 <- pladmm(musa_trials_data[musa_plt_nodes == "3", ],
                           ~  status_2, 
                           data = genotypes_info,
                           rho = 82)

summary(pladmm_node_3_s2)

######################################

#Breeding programs comparison + landraces

#node 2
pladmm_node_2_bp <- pladmm(musa_trials_data[musa_plt_nodes == "2", ],
                          ~  BP_ ,
                          data = genotypes_info,
                          rho = 82)

summary(pladmm_node_2_bp)

#node 3
pladmm_node_3_bp <- pladmm(musa_trials_data[musa_plt_nodes == "3", ],
                           ~  BP_ ,
                           data = genotypes_info,
                           rho = 154)

summary(pladmm_node_3_bp)



anova(base_2, pladmm_node_2_s1)

anova(base_2, pladmm_node_2_bp)

anova(base_3, pladmm_node_3_bp)


####################################
node_2 <- musa_trials_data[musa_plt_nodes == "2", ]
node_2 <- gosset:::pseudo_rank(node_2)
nd2_pl <- PlackettLuce::PlackettLuce(node_2)
summary(nd2_pl)


node_3 <- musa_trials_data[musa_plt_nodes == "3", ]
node_3 <- gosset:::pseudo_rank(node_3)
nd3_pl <- PlackettLuce::PlackettLuce(node_3)
s3 <- summary(nd2_pl)



