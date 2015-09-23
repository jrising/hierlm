# hierlm
Hierarchical Linear Model

The `hierlm` package allows researchers to perform a large range of
hierarchical models, in which parameters are related to eachother or
to 'hyper'-parameters.  Coefficients are modeled as being drawn from
Gaussian distributions, whose mean is estimated simultaneously with
the value of those coefficients.  The technique is very similar to
Bayesian Hierarchical Modeling (See Bayesian Data Analysis, chapter 5,
by Gelman et al.), but because it uses a linear model (OLS) can handle
much larger datasets.

For more information, see the vingette and top-level `hierlm()`
function documentation.

Terminology Note: Traditionally, "Hierarchical linear modeling" and
"multilevel modeling" describe models where coefficients can be
described with interactions with higher-level data.  This package does
not do multilevel modeling, since both the coefficients and the data
in `hierlm` can describe more than one level.

# Installation

Install this package from CRAN as normal, or get the code directly from the github repository.  To install from github, first make sure you have the devtools package, and run the following:
```
library(devtools)
install_github("hierlm", username="jrising")
library(hierlm)
```
