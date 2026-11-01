# Which measured traits explain preference?

**Status** tested-on-synthetic-data
**Decision record** [trait associations](../../decisions/trait-associations.md) and [genetic gain](../../decisions/genetic-gain.md)
**Domain leads for genetic parameters** Joost van Heerwaarden and Hugo Dorado-Betancourt (Wageningen University)
**Derived from** [archive/rtbfoods-eba-nigeria](../../archive/rtbfoods-eba-nigeria/) (`02_estimate_blups.R`, `03_pladmm.R`) and [archive/teff-elife](../../archive/teff-elife/) `01_farmers_oa.R`

## Scientific question

Which traits measured in the lab or on station (texture, colour, yield components, phenology) explain why participants prefer some genotypes over others, and by how much?

## Intended use

Use it to link breeding or food-science measurements to participant preference, for example to define product profiles or decide which lab traits to screen for. The genotype estimates of the measured traits become item covariates of a Plackett-Luce model (`PlackettLuce::pladmm`).

It does not fit when the traits were ranked by participants. Use the trait-association use case for that. It also does not estimate the genetic signal in preference. For that, see the two-step approach of Dorado-Betancourt et al. (2026).

## Input data

- Tricot rankings of the genotypes in the ClimMob format.
- Lab or station data with one row per sample, a genotype column, the trait columns and, when available, the region or location, processor, batch and sample identifiers.

Genotype names must match between the two sources. The workflow warns about any genotype found in only one of them.

## Assumptions

- Lab samples are replicates of the genotypes participants evaluated, processed in a comparable way.
- Trait effects are additive and linear on the log-worth scale.
- When processor, batch or sample information is missing, sample variation within a genotype is treated as residual.

## Approach

1. Check that genotype names match.
2. For each trait, estimate the BLUE (adjusted genotype mean) with a fixed-effect model and the BLUP with a mixed model with genotype as random. Region enters both models as a fixed effect when genotypes were measured in more than one region. When each genotype was measured in only one region, the workflow warns that region and genotype are confounded. Estimates are matched to genotypes by name.
3. Report the repeatability of each trait, Vg / (Vg + Ve / n).
4. Screen the covariates with variance inflation factors and pairwise correlations. Traits computed from others, such as gumminess and chewiness in texture profile analysis, should not enter together with their components.
5. Standardize the covariates so coefficients are per standard deviation.
6. Fit `pladmm`, test it against the free Plackett-Luce model, and redraw the covariates within their standard errors 50 times to see how stable the coefficients are.

## What changed from the archived scripts

- `02_estimate_blups.R` builds a table with the genotype levels and then appends each trait's BLUPs with `cbind`. That only works if `ranef()` returns genotypes in exactly that order and every genotype has data for every trait. This version merges by name.
- BLUPs went into `pladmm` on their raw scale. BLUPs shrink each trait towards zero according to its repeatability, so a poorly measured trait gets a smaller covariate range and a larger coefficient. This version uses standardized BLUEs by default. With equal samples per genotype, standardized BLUPs give exactly the same model. They differ when sample numbers are unequal.
- The correlation filter was commented out, and gumminess and chewiness entered together with hardness and cohesiveness. Gumminess is hardness times cohesiveness and chewiness is gumminess times springiness, so their coefficients cannot be separated. This version flags them.
- The `pladmm` step itself is unchanged.

## Alternatives

- `pltree` with the same worth formula, to let trait effects differ between participant groups, as in `archive/rtbfoods-eba-nigeria/03_pladmm.R`.
- Stacking several ranked traits (taste, mouldability, smoothness) as replicated rankings, as in the archived script. This assumes one common preference behind all of them. Fit each trait separately first to check that the effects agree.

## Interpretation

On the synthetic data (14 genotypes, 3 lab samples per genotype in each of two regions, 900 consumers), every trait except soluble content has repeatability of at least 0.96. Soluble content has 0.26. Including gumminess and chewiness pushes the VIFs above 100 for all texture traits. After dropping them, all VIFs are below 2.

The model recovers the simulated effects. Cohesiveness gives +0.71 per standard deviation (simulated +0.8), hardness −0.53 (−0.6) and colour +0.44 (+0.4), all with narrow intervals and stable signs across redraws. Two results show why caution is needed with few genotypes. First, springiness, simulated to have no effect, gets −0.16 (interval −0.24 to −0.07). Second, the test against the free model is borderline (p = 0.03). Both are consistent with small estimation errors in the large-effect traits leaving structure the model assigns elsewhere. With 900 consumers, small errors at genotype level become detectable. Soluble content gets +0.08, interval 0.00 to 0.16, and its sign holds in 90% of redraws. Treat it as no clear effect.

On the raw scale the soluble coefficient is 0.07 with BLUEs but 0.27 with BLUPs, almost four times larger, purely because of shrinkage. Standardized, the two give identical results.

## Limitations

- With few genotypes, only a few traits can enter at once. The workflow stops when fewer than 2 degrees of freedom remain.
- Measurement error in the covariates biases coefficients and can create small spurious effects. Report the sensitivity table.
- Associations are not causal. A measured trait may stand for an unmeasured one.

## References

- Alamu et al. 2023, *J Sci Food Agric*, and Olaosebikan et al. 2023, *J Sci Food Agric*, link lab texture and colour of eba to tricot consumer preferences. See the [tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base) cards `alamu-2023-jsfa` and `olaosebikan-2023-jsfa`.
- Dorado-Betancourt et al. 2026, *Theor Appl Genet*, for genetic parameters from rankings.

## Files

- `script/helper/functions.R` holds the documented functions.
- `script/01-analysis.R` is the workflow. Open the `.Rproj` file and run it, or run `Rscript script/01-analysis.R` from this folder.
- `script/helper/simulate-data.R` simulates lab and consumer data for the same genotypes.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
