# .........................................
# .........................................
# Compare yield measures in cassava plots 
# collected by farmers and technicians in Nigeria
# First run Mar 2024
library("tidyverse")
library("ggplot2")
library("gosset")
library("PlackettLuce")
library("car")

dat = read.csv("data/tricot-data-nextgen-cassava.csv")

x = dat[,c("state", paste0("variety", letters[1:3]), paste0("rootweight_plot_", letters[1:3]))]

x = na.omit(x)

x$enumerator = ifelse(x$state == "Imo", "technician", "farmer")

table(x$enumerator)


yield = data.frame(state = rep(x$state, each = 3),
                   enumerator = rep(x$enumerator, each = 3),
                   variety = unlist(x[,paste0("variety", letters[1:3])]),
                   yield = unlist(x[,paste0("rootweight_plot_", letters[1:3])]))

yield = yield[yield$yield != 0, ]

yield$logyield = log10(yield$yield)

ggplot(yield, aes(y = variety, x = logyield, fill = enumerator)) +
  geom_boxplot() +
  #facet_wrap( ~ bio, scale = "free") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "")

ggsave("output/boxplot-root-weight.pdf",
       height = 10,
       width = 8)



mod = lm(logyield ~ enumerator + variety, data = yield)

summary(mod)




