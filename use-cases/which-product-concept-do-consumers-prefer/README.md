# Which product concept do consumers prefer?

**Status** tested-on-synthetic-data
**Decision record** [consumer and concept testing](../../decisions/consumer-concept-testing.md)
**Derived from** [archive/rtbfoods-consumer-testing](../../archive/rtbfoods-consumer-testing/), [archive/maize-concept-testing](../../archive/maize-concept-testing/), [archive/mip-concept-testing](../../archive/mip-concept-testing/), [archive/teff-elife](../../archive/teff-elife/)

## Scientific question

Which attributes of a product concept (price, cooking time, packaging, nutrition) drive consumer preference, by how much, and do consumer groups weigh them differently?

## Intended use

Use it in product-development research where concepts or food products are described by attributes and consumers rank three each, as in tricot. The design should stay comparable to tricot, with single-stage ranking and few attributes.

It does not estimate adoption, market share or willingness to pay.

## Input data

- Rankings in the ClimMob tricot format, with concept names in `item_A`, `item_B`, `item_C`.
- An attribute table with one row per concept and a `concept` column with the same names.
- Consumer covariates, such as urban or rural.

Record how concepts were presented (description, picture, video, tasted product).

## Assumptions

- Attribute effects add up on the log-worth scale. Interactions must be written into the formula.
- The attribute table is matched to the rankings by concept name. `pladmm()` reads the rows of its data in the order of the ranking columns, and `rank_tricot()` orders items by first appearance, so matching by position silently assigns the wrong attributes. `align_features()` prevents this.

## Approach

1. Build rankings and fit a free Plackett-Luce model, one worth per concept, as a description.
2. Fit log-worth as a linear function of attributes with `PlackettLuce::pladmm()`.
3. Test whether the attributes explain the concept worths, comparing the attribute model with the free model (likelihood ratio, both without pseudo-rankings).
4. Fit the attribute model by consumer group and test whether the coefficients differ.
5. Predict the relative preference of new attribute combinations against a tested concept, refusing any level outside those tested.

## Alternatives

- Plackett-Luce trees with the attribute model in the nodes, to find consumer groups from covariates rather than defining them in advance, as in the RTBfoods analysis.
- Choice-based conjoint designs with mixed logit models, when attributes are designed orthogonally and each consumer completes many choice tasks.

## Interpretation

On the synthetic data (900 consumers, 12 concepts, four attributes), the attribute effects are recovered. Each price step lowers log-worth by 0.64, long cooking by 0.72, branded packaging adds 0.41 and biofortification 0.44, all close to the simulated values. The additive model is adequate (deviance 5.75 on 7 df, p = 0.57). Urban consumers are about twice as price sensitive as rural consumers (−0.92 against −0.41, test of difference p < 0.001). A new short-cooking, branded, biofortified concept at the lowest price would be preferred over Concept D with probability 0.85.

These are relative preferences among the concepts tested. Adoption and market potential need additional evidence.

## Limitations

- Predictions are only supported within the tested attribute levels.
- When the adequacy test rejects the additive model, look for interactions or missing attributes before interpreting coefficients.
- Standard errors for the intercept are not reported, since it is not identified in a ranking model.

## Files

- `functions.R` holds the documented functions.
- `analysis.R` is the workflow, run with `Rscript analysis.R` from this folder.
- `data/simulate-data.R` simulates the concept test.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
