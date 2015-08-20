# hierlm
Hierarchical Linear Model

The `hierlm` package allows researchers to perform a large range of
hierarchical models, in which parameters are related to eachother or
to 'hyper'-parameters.  The technique is very similar to Bayesian
Hierarchical Modeling (See Bayesian Data Analysis, chapter 5, by
Gelman et al.), but because it uses a linear model (OLS) can handle
much larger datasets.

For more information, see the vingette and top-level `hierlm()`
function documentation.

# Installation

Install this package from CRAN as normal, or get the code directly from the github repository.  To install from github, first make sure you have the devtools package, and run the following:
```
library(devtools)
install_github("hierlm", username="jrising")
library(hierlm)
```
