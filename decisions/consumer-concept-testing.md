# Consumer and concept testing

Status: methodological rationale from Kauê de Sousa (Bioversity International), 2026-10-31, expanded 2026-11-06 from the archived scripts and the published studies listed under sources. Points marked (script) are read from the code and await Kauê's confirmation. This is not independent validation of cited studies or software.

## Two kinds of test

- **Product testing.** Participants taste or use a real product, a boiled sweetpotato, eba from a cassava genotype, and rank three. The ranking says which genotype people prefer when they meet the product. Moyo et al. (2021) is the first tricot consumer test, Olaosebikan and Alamu (2023) the cassava ones.
- **Concept testing.** Participants watch or read a description of a product that does not exist yet, a future maize hybrid with a bundle of attributes, and rank three concepts. The ranking says which bundle of attributes people say they want. Rutsaert, Mbugua, Donovan (2023 to 2025) with CIMMYT are the video-based concept tests. A preferred concept is a stated preference about a description, with none of the constraints of buying or growing.
- **Same engine, different claims.** Both share the tricot engine, random sets of three, best and worst, Plackett-Luce, trees and reliability. What a result supports differs, and the record says which kind of test it is before any conclusion.

## Default approach

- **Design stays comparable to tricot.** Single-stage ranking, each participant ranks one set of three, few traits beside overall preference. Where the design departs (many items per participant, repeated tasks, Likert scores), use [data synthesis](data-synthesis.md) for ratings or the conjoint alternative below.
- **Formats.** Centralised tasting under controlled preparation, and home tasting under the participant's own preparation. Pool them when the rankings agree, and say which format each conclusion rests on. Pooling made the advantage of the top sweetpotato clone over the check significant in Ghana (Moyo et al. 2021).
- **Traits.** Overall preference is the outcome. Sensory traits (taste, texture, colour, odour, firmness) are ranked at the same moment and related to overall preference with the trait workflow (see [trait associations](trait-associations.md)). Some sensory traits give a network that is not connected, so rankings are built trait by trait and traits with broken networks are dropped (script, cassava).
- **Overall technology performance.** When several traits are ranked by the same participant, their rankings can be stacked into one grouped ranking per participant, weighted by response, to estimate an overall performance of each product across traits (script, cassava). Report it next to overall preference, not instead of it.
- **Concept attributes.** A concept is a bundle of attribute levels. Model log worth as a linear function of the attributes with `PlackettLuce::pladmm()`, test the additive model against the free model, and predict the relative preference of untested bundles only within the tested levels (use case `which-product-concept-do-consumers-prefer`). Match attributes to concepts by name, never by position.
- **Who prefers what.** Split by predefined groups (country, gender, urban or rural) and test the split with the likelihood ratio (`gosset::likelihood_ratio`), or fit a tree on participant covariates. Reliability against a check is reported per group.
- **Market segments.** Segments are built from socio-economic covariates without the preference outcome (FAMD, one-hot encoding, k-means, elbow for k), named from their covariate profiles, and then tested against the rankings (script, MIP). Compare the segment model with an intercept-only model, a gender model and a tree on the raw covariates by blocked cross-validation (`gosset::crossvalidation`), and keep the segments only when they predict rankings better than the simpler models (script, `03-model-validation.R`). The rules in [socioeconomic heterogeneity](socioeconomic-heterogeneity.md) apply.
- **Open answers.** Reasons for choosing or rejecting a product are text. Clean, tokenise and count terms per product, draw word clouds and co-occurrence networks of terms, and run a correspondence analysis of terms against products and groups (script, maize and sweetpotato). Text analysis explains the rankings, it does not replace them.
- **Alpha.** As in [climate and environment](climate-environment.md), 0.01 for large data sets or when the case needs a conservative threshold, 0.05 or 0.10 in ordinary studies, always stated.

## Alternatives

- Choice-based conjoint with mixed logit, when attributes are designed orthogonally and each participant completes many choice tasks.
- Bradley-Terry models on pairwise choices, when the task is a pair rather than a triplet.
- Hedonic scores analysed as ratings, when the panel is trained and the scale is fine (see ratings in [data synthesis](data-synthesis.md)).
- Revealed preference designs, a mock agro-dealer where farmers buy with real money (Mawia et al. 2025), when the question is what people choose rather than what they say.

## What the results cannot support

- Rankings give relative preference among the products or concepts tested. They do not give adoption, market share, willingness to pay or purchase intent. Mawia et al. (2025) show farmers choosing the decades-old market leader in a shop whatever they say they prefer.
- A preferred concept is a preference about a description. Attributes people did not see cannot be in the result, and attribute levels outside the tested range cannot be predicted.
- Centralised tastings reach the people free at that time, and home tastings lose control of preparation. Say whom each sample represents.
- A segment that looks distinct on a worth plot is not distinct until the test says so, and a segment that predicts no better than gender is not worth naming.

## Sources

- [Moyo et al. 2021, Frontiers in Sustainable Food Systems](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/moyo-2021-fsufs.md)
- [Olaosebikan et al. 2023, JSFA](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/olaosebikan-2023-jsfa.md) and [Alamu et al. 2023, JSFA](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/alamu-2023-jsfa.md)
- [Rutsaert et al. 2023, market intelligence brief](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/rutsaert-2023-mib.md)
- [Mbugua et al. 2024, market intelligence brief](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/mbugua-2024-mib.md)
- [Donovan et al. 2025, Experimental Agriculture](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/donovan-2025-expagric.md)
- [Mawia et al. 2025, Frontiers in Sustainable Food Systems](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/mawia-2025-fsufs.md)

## Archive examples

- `archive/maize-concept-testing/` (Plackett-Luce by country and gender, text analysis, correspondence analysis, co-occurrence networks)
- `archive/mip-concept-testing/` (segments from covariates, model validation by cross-validation, likelihood ratio by group)
- `archive/rtbfoods-consumer-testing/` (trait-by-trait rankings, overall technology performance, pladmm and trees with participant covariates)
- `archive/rtbfoods-eba-nigeria/` (lab trait BLUPs as item covariates)
- `archive/moyo-et-al-sweetpotato/` (consumer tasting, text analysis)
