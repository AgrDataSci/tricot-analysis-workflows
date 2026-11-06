library(psychotree)

musa_paircomp <- musa_paircomp[1:48,]

bpc_data <- cbind(musa_paircomp, clim_dat_imtp)


btt <- psychotree::bttree(musa_paircomp ~ MLDS + MLWS + R10mm + R20mm + Rx1day + Rx5day + R95p, 
                          data = bpc_data,
                          verbose = T,
                          breakties = T,
                          alpha = 0.9,
                          #minsize = 24,
                          cores = 4,
                          bonferroni = F,
                          na.action = na.pass)



summary(btt)

worth(btt)

psychotree:::node_btplot()

ggparty::ggparty(btt)

ggparty::ggparty(btt) +
  geom_edge() +
  geom_edge_label() +
  geom_node_label(aes(label = splitvar),
                  ids = "inner") +
  geom_node_label(aes(label = info),
                  ids = "terminal")

bt <- btmodel(musa_paircomp)

unclass(bpc)

summary(bt)

plot(bt)

worth(bt)


bt_reg <- btReg.fit(bpc)

summary(bt_reg)

plot(bt_reg)

worth(bt_reg)