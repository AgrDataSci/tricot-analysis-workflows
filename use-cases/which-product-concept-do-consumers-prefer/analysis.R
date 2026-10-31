# Which product concept do consumers prefer?
# Status: tested-on-synthetic-data
# Decision record: decisions/consumer-concept-testing.md
# Derived from: archive/rtbfoods-consumer-testing, archive/maize-concept-testing,
#   archive/mip-concept-testing, archive/teff-elife
#
# Run from this folder with  Rscript analysis.R

library("PlackettLuce")
library("gosset")

source("functions.R")
source("data/simulate-data.R")

dir.create("outputs", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
sim = simulate_concept_test()
features = sim$features

rk = build_rankings(sim$data, c("item_A", "item_B", "item_C"),
                    c("overall_pos", "overall_neg"))
R = rk$rankings
group = sim$data$group[rk$valid]

PlackettLuce::connectivity(R, verbose = FALSE)$no

# 2. Worth per concept, no assumption on attributes -----------------------
free = PlackettLuce(R)
worth = sort(stats::coef(free, log = TRUE), decreasing = TRUE)
print(round(worth - mean(worth), 2))

# 3. Attribute model ------------------------------------------------------
form = ~ price + cooking + packaging + biofortified

mod = fit_attribute_model(R, form, features)

ae = attribute_effects(mod)
print(ae, digits = 2)

# 4. Do the attributes explain the concept worths? ------------------------
print(attribute_adequacy(R, mod), digits = 3)

# 5. Do consumer groups weigh attributes differently? ---------------------
bg = attribute_effects_by_group(R, group, form, features)

print(bg$effects, digits = 2)
print(bg$test, digits = 3)

# 6. Predict untested combinations ----------------------------------------
new = data.frame(concept = c("New 1", "New 2"),
                 price = c(1, 2),
                 cooking = factor(c("short", "short"), levels = c("short", "long")),
                 packaging = factor(c("branded", "sack"), levels = c("sack", "branded")),
                 biofortified = factor(c("yes", "yes"), levels = c("no", "yes")))

pc = predict_concepts(mod, features, new, reference = "Concept D")
print(pc, digits = 2)

# 7. Outputs --------------------------------------------------------------
utils::write.csv(ae, "outputs/attribute-effects.csv", row.names = FALSE)
utils::write.csv(bg$effects, "outputs/attribute-effects-by-group.csv", row.names = FALSE)
utils::write.csv(pc, "outputs/predicted-concepts.csv", row.names = FALSE)

sessionInfo()
