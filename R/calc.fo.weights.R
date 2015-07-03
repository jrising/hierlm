#' Calculate Fictional Observation Weights
#'
#' Determines the coeffiicients for terms in the fictional observations,
#' to ensure a given weight relative to the normal observations.
#'
#' @param formula lm-style formula
#' @param label the term to be used in fictional observations
#' @param ratio desired ratio of observed and fictional data
#' @param data optional data frame
#' @return Coefficients for fictional observations
#' @examples
#' calc.fo.weights(Sepal.Length ~ Petal.Length + Sepal.Width : Species, "Sepal.Width:Species", 5, data=iris)

calc.fo.weights <- function(formula, label, ratio, data) {
    mod.lm <- lm(formula, data)
    sumerr <- sum(mod.lm$residuals^2)

    ## Collect the beta values for all of these
    dummies <- substr(names(mod.lm$coefficients), 1, nchar(label)) == label
    if (sum(dummies) == 0 && grepl(':', label)) {
        ## Try flipping an interaction
        subterms = split.interaction.term(label)
        dummies <- grep(paste(subterms[2], ".*", subterms[1], sep=""), names(mod.lm$coefficients))
    }
    betas <- mod.lm$coefficients[dummies]

    sumerr / (ratio * sum(dummies) * var(betas))
}
