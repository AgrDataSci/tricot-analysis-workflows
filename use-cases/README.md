# Use cases

Generic, question-first workflows. Each use case answers one scientific question, links to its decision record in `decisions/`, and runs on synthetic data.

Each use case is a self-contained RStudio project with the layout of [template-repo-data-analysis](https://github.com/AgrDataSci/template-repo-data-analysis), plus a `tests/` folder. Open its `.Rproj` file, then run `script/01-analysis.R` or `Rscript tests/run-tests.R`. All paths are relative to the use-case folder.

To add a use case, copy `_template/` to a new folder named after the question (for example `which-traits-drive-preference/`), rename its `.Rproj` file to match, and fill in every section.
