# Use case title as a question

**Status:** historical | reviewed-not-executed | tested-on-synthetic-data | validated-against-original-results
**Decision record:** link to the file in `decisions/`
**Derived from:** link to the folder in `archive/` and the related publication

## Scientific question

What the user wants to know, in one or two sentences.

## Intended use

When this workflow fits, and when it does not.

## Input data

Required variables, formats and metadata (observer, instrument, scale, timing, units).

## Assumptions

Experimental design and statistical assumptions the analysis relies on.

## Approach

The steps, and why each method was chosen.

## Alternatives

Other defensible methods and when to prefer them.

## Interpretation

What the results support, and what they do not.

## Limitations

Known weaknesses and open questions.

## Folder structure

The layout follows [template-repo-data-analysis](https://github.com/AgrDataSci/template-repo-data-analysis), with a `tests/` folder added.

```text
<use-case>/
├── <use-case>.Rproj          # open in RStudio, sets the working directory here
├── README.md                 # this file
├── data/                     # input data, or a README describing it
├── script/
│   ├── 01-analysis.R         # the workflow
│   └── helper/
│       ├── functions.R       # documented functions
│       └── simulate-data.R   # synthetic data with a known answer
├── tests/
│   └── run-tests.R           # Rscript tests/run-tests.R
├── output/                   # results, not tracked by git
└── references.bib
```

All paths are relative to the use-case folder. Rename `_template.Rproj` when you copy this folder.

## References

See `references.bib`.
