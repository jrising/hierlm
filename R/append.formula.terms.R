#' Add additional terms to a formula
#'
#' @param formula an lm-style formula
#' @param terms a vector of characters
#' @return Augmented formula
#' @examples
#' append.formula.terms(y ~ a + b, c("b", "c", "d"))

append.formula.terms <- function(formula, terms) {
    chunks1 <- as.character(formula)
    terms1 <- get.formula.terms(formula)

    for (term in terms)
        if (!(term %in% terms1))
            terms1 <- c(terms1, term)

    as.formula(paste(chunks1[2], "~", paste(terms1, collapse=" + ")),
               env=attr(formula, '.Environment'))
}

