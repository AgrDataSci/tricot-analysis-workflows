# Do on-farm and on-station results agree?

**Status** tested-on-synthetic-data
**Decision record** [genetic gain and station/farm comparisons](../../decisions/genetic-gain.md)
**Derived from** [archive/wageningen-genetic-gain-cassava](../../archive/wageningen-genetic-gain-cassava/), [archive/wageningen-genetic-gain-groundnut](../../archive/wageningen-genetic-gain-groundnut/), [archive/potato-kang-et-al](../../archive/potato-kang-et-al/)

## Scientific question

Are the genotypes that perform best on station the ones farmers prefer on their farms, and how reliable is each source of information?

## Intended use

Use it to judge whether station selection is a good proxy for farmer preference, where on-farm testing adds information, and how many participants an on-farm trial needs for a reliable ranking.

It does not estimate the heritability of farmer preference. That is a research proposition, and it needs a design that separates genetic, environmental, rater and measurement effects.

## Input data

- Station trials, one row per plot, with `yield` (or another trait), `genotype`, `location` and `rep`, in a randomized complete block design within locations.
- Tricot rankings of overall preference for the same genotypes.

Genotype names must be harmonized between the two sources. The workflow warns about genotypes that appear in only one.

## Assumptions

- Station data follow an RCBD within each location.
- Entry-mean heritability uses the number of locations and the mean replicates per location, both counted from the factors used in the model.
- On-farm participants are independent.

## Approach

1. Fit `yield ~ location + (1 | location:rep) + (1 | genotype) + (1 | genotype:location)` with `lme4::lmer`.
2. Compute entry-mean H2 = Vg / (Vg + Vgl / L + Ve / (L r)) and genotype BLUPs.
3. Fit a Plackett-Luce model to the on-farm rankings and centre the log-worth on its mean.
4. Estimate the reliability of the on-farm ranking. Split participants into random halves 50 times, correlate the two log-worth vectors and apply the Spearman-Brown correction. This is the reliability of the preference estimate, not a heritability.
5. Compute the Spearman correlation between station BLUPs and on-farm log-worth, with a bootstrap interval over genotypes, and the overlap of the top 5 genotypes from each source.

## What changed from the archived scripts

The archived cassava heritability script counts locations from a different column (`state`) than the one in the model (`Zone`). This version counts locations and replicates from the same factors used in the model, so the H2 denominator matches the variance components.

## Alternatives

- Generalized (Cullis) heritability from the prediction error variance, for unbalanced station data.
- A genetic correlation from a multivariate mixed model, when station and on-farm data share a design that supports it.

## Interpretation

On the synthetic data (15 genotypes, 3 locations by 3 replicates on station, 600 tricot participants), station H2 is 0.88 (simulated value 0.83). The on-farm ranking is highly reliable (split-half 0.97, Spearman-Brown 0.98). Station BLUPs and on-farm log-worth agree moderately (Spearman 0.65, 95% interval 0.18 to 0.89), and 3 of the top 5 genotypes coincide. With 15 genotypes the interval is wide. G13 shows how this matters. It is mid-ranked on station but the most preferred on-farm, which is the kind of genotype on-farm testing exists to find.

A low correlation does not show either source is wrong. Farmers may value traits not measured on station, and on-farm environments differ from stations.

## Limitations

- With few genotypes the correlation interval is wide. Report it.
- Entry-mean H2 assumes a balanced design. Use Cullis H2 otherwise.
- The split-half reliability depends on the number of participants per genotype. It is not H2.

## Files

- `functions.R` holds the documented functions.
- `analysis.R` is the workflow, run with `Rscript analysis.R` from this folder.
- `data/simulate-data.R` simulates station and on-farm data for the same genotypes.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
