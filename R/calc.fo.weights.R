#' Calculate Fictional Observation Weights
#'
#' Determines the coeffiicients for terms in the fictional observations,
#' to ensure a given weight relative to the normal observations.
#'
#' @param ratios desired ratio of observed and fictional data
#' @param formula lm-style formula
#' @param data optional data frame
#' @return Coefficients for fictional observations
#' @examples
#' calc.fo.weights(5, Sepal.Length ~ Petal.Length + Sepal.Width : Species, data=iris)

calc.fo.weights <- function(ratios, formula, data) {
    mod.lm <- lm(formula, data)
    sumerr <- sum(mod.lm$residuals^2)

    weights <- c()

    ## Look for factor items
    labels <- attr(mod.lm$terms, 'term.labels')
    for (label in labels[grep(":", labels)]) {
        ## Collect the beta values for all of these
        dummies <- substr(names(mod.lm$coefficients), 1, nchar(label)) == label
        betas <- mod.lm$coefficients[dummies]

        weight <- sumerr / (ratios[length(weights)+1] * sum(dummies) * var(betas))
        weights <- c(weights, weight)
    }

    weights
}
