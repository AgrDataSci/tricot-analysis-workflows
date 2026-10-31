# Genetic gain and station/farm comparisons

Status: methodological rationale from Kauê de Sousa (2026-10-31), adapted from the AgrDataSci decision record. This is not independent validation of cited studies or software.

**Domain leads** Joost van Heerwaarden and Hugo Dorado-Betancourt (Wageningen University). Build on their work before extending methods in this domain.

- Questions include genotype-by-environment interactions, decision support, return on investment, and the correspondence between on-station and on-farm heritability.
- Investigate overall farmer preference as a potentially heritable breeding trait. This is a methodological research proposition, not proof that Plackett-Luce worth estimates have heritability.
- Reference method for genetic parameters from rankings. Dorado-Betancourt et al. (2026, *Theor Appl Genet*), in work led by Joost van Heerwaarden, estimate genotype means and variances from Plackett-Luce or Thurstonian models and feed them into weighted two-step mixed models to estimate variance components and heritability. Their code is at [hdorado/tricot-ranking-analysis](https://github.com/hdorado/tricot-ranking-analysis). Use it rather than ad hoc approaches, and respect its stated limits. Cullis heritability can be negative in some trials, heritability below 0.25 tends to be overestimated, and about 30 replicates per genotype are usually needed.
- Distinguish latent preference, observed rankings, genetic effects, environment, rater effects and measurement error. Specify design and variance-component requirements before making H2 claims.

## Sources

- [Dorado-Betancourt et al. 2026, TAG](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/dorado-2026-tag.md)

## Archive examples

- `archive/wageningen-genetic-gain-cassava/`
- `archive/wageningen-genetic-gain-groundnut/`
- `archive/potato-kang-et-al/`
