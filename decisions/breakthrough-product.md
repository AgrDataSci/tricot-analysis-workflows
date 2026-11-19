# Breakthrough products

Status: draft rationale prepared with Kauê de Sousa (Bioversity International), 2026-11-20. Not yet reviewed by him or by the domain leads. This is not independent validation of cited studies or software.

**Domain leads** Joost van Heerwaarden and Hugo Dorado-Betancourt (Wageningen University) lead genetic gain and genetic parameters from rankings. Ask them to review this record before its use case moves past a draft.

- **Definition.** A breakthrough product is a variety with at least 25% higher productivity or value in farmers' fields. The definition comes from Cairns et al. (2026), who cite Coaldrake and Werner (2025) for it. The threshold is an argument of the workflow, not a constant.
- **Where the decision belongs.** A breakthrough decision is generally taken with station data, from replicated breeding trials across locations. maniva does not connect station data yet. In tricot-data the only trials with on-station companions are the five groundnut trials from Tanzania, and their station files are not part of the release. Until station data are connected, this workflow gives the on-farm side of the evidence only. Say so in every answer that uses it.
- **Decision rule.** Reliability, from Eskridge and Mumm (1992), is the probability that a test variety outperforms the check across the environments where the check is grown. It weighs the mean difference against its stability, and breeders, growers and managers can read it without knowing what a true mean is.
- **Our extension.** Eskridge and Mumm set the threshold at zero. We move it to the required gain, so the reliability becomes the probability that the gain is at least 25% on a farm. This is our inference, not part of their paper.
- **Two probabilities, never merged.** Report the confidence that the average gain reaches the threshold, and the reliability at the threshold. The first is about the mean over farms. The second is about one farm. In the synthetic data of the use case, a variety with a true mean gain of 40% reaches 25% on 59% of farms. Both numbers go in the table, each under its own name.
- **Estimation in two steps.** Follow the two-step logic of Dorado-Betancourt et al. (2026), which they developed for rankings. Step 1 estimates, per trial, the mean of each variety against the check and its variance. Step 2 combines the trials with weights from those variances and a variety-by-trial variance. For measured yield, step 1 is a model of log yield on variety and farm. Applying their logic to measured yield is our adaptation. Their code remains the reference for heritability and variance components.
- **Estimators of reliability.** The default is model-based, from the mean and the standard deviation of the differences (their equation 2). The direct estimator is the share of farms that grew both the variety and the check where the variety won. Eskridge and Mumm need about 16 environments to be within 0.15 of a true reliability of 0.85. tricot farms grow three varieties, so few farms hold a given variety beside the check. Report the direct share with its number of pairs as a cross-check, and trust it only above about 16 pairs.
- **One variance for all varieties.** With few direct pairs, the model-based reliability uses one residual variance for all varieties. It then cannot show that one variety is less stable than another, which is the feature Eskridge and Mumm value most. State this, and use the direct share where pairs allow.
- **Zero yields.** A model of log yield drops plots with zero yield. Count them, report the share of failed plots per variety, and keep them in the direct share, where a failed plot is a clear loss. If the share of failed plots differs between a variety and the check, the gain among harvested plots overstates or understates the real difference. Report both.
- **The check.** The choice of check can change the verdict. A check that is poorly adapted to the trial environments gives unrealistic reliabilities. When a trial names several checks, or the breeder is unsure, run each one and report all.
- **Value.** The definition covers productivity or value. Rankings cannot return a percentage gain. Report the probability that farmers rank the variety above the check on overall preference, from a Plackett-Luce model, beside the yield gain. Do not convert it into a percentage, and do not call a variety a breakthrough product on preference alone until a rule for value is agreed.
- **Replication.** Give no verdict below a minimum number of plots per variety. The default of 30 borrows the guidance of Dorado-Betancourt et al. (2026) for ranking data. It has not been tested for measured yield.
- **Verdict.** "Supported" when the confidence that the average gain reaches the threshold is at least 0.8. "Indicative" when the point estimate reaches the threshold but that confidence is lower. "Not supported" otherwise. "Insufficient data" below the minimum replication. These cut-offs are provisional and wait for a decision by Kauê de Sousa and the domain leads.

## Alternatives

- One mixed model on all plots, with variety, trial, farm and variety-by-trial terms. Prefer it when trials share most varieties and the software handles the unbalanced design.
- A Bayesian model that returns the full probability of outperforming a check or a threshold, as in ProbBreed. It is outside maniva and has no card in the knowledge base yet.
- Arithmetic yield ratios with zeros kept. They answer a question about total production and are driven by a few extreme plots.
- Stability statistics (regression coefficient, deviation mean square, stability variance). Eskridge and Mumm found them less repeatable than reliability.

## What the results cannot support

- A release or advancement decision on their own. On-farm evidence is one side. Station data are the other.
- A statement about farms unlike those in the trials, or about a check outside its area of adaptation.
- A ranking of varieties by reliability alone. Eskridge and Mumm found reliability less repeatable across sets of environments than the mean, so the mean gain stays in the table.
- Adoption, market share or profit. A yield gain is not a value gain, and economic factors are outside the method.
- Yields derived from an assumed plot area are safe only if the three plots of a farm had the same size. The ratio within a farm then does not depend on the area.

## Open questions

- The cut-offs of the verdict, and whether the minimum of 30 plots fits measured yield.
- A rule for value, the second half of the definition.
- How to bring station data in, and how to weigh station and on-farm evidence in one decision.
- Whether a two-part model, the chance of harvesting anything and then the yield given a harvest, should replace the count of failed plots.
- Variety-specific stability from on-farm data, which needs more direct pairs or a model for the variance.

## Use case

- `use-cases/is-this-variety-a-breakthrough-product/`

## Source pointers

- [Eskridge and Mumm 1992, TAG](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/eskridge-1992-tag.md), card proposed on the same date as this record
- [Dorado-Betancourt et al. 2026, TAG](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/dorado-2026-tag.md)
- [Cairns et al. 2026, J Exp Bot](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/cairns-2026-jxb.md)
- [de Sousa et al. 2023, SoftwareX, gosset](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/desousa-2023-softwarex.md), for `reliability()` on rankings
