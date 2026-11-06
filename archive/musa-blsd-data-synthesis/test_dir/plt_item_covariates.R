library(PlackettLuce)
library(prefmod)

data("salad", package = "prefmod")

head(salad)

features <- data.frame(salad = LETTERS[1:4],
                       acetic = c(0.5, 0.5, 1, 0),
                       gluconic = c(0, 10, 0, 10))

head(features)



standardPL <- pladmm(salad, ~ salad, data = features, rho = 8)
summary(standardPL)

logLik(standardPL) * 0.1

set.seed(1)

judge_features <- data.frame(varC = rpois(nrow(salad), lambda = salad$C^2))

grouped_salad <- group(as.rankings(salad), 1:nrow(salad))

tree <- pltree(grouped_salad ~ .,
               worth = ~acetic + gluconic,
               data = list(judge_features, features),
               rho = 2, minsize = 10)


plot(tree, ylines = 2)



