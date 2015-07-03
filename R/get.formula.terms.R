#' Extract the terms from an lm formula
#'
#' Return a list of terms in an lm formula, in the order they appear
#'
#' @param formula lm-style formula
#' @return Vector of the terms in the lm formula
#' @examples
#' get.formula.terms(Sepal.Length ~ Petal.Length + Species + Sepal.Width : Species)

get.formula.terms <- function(formula) {
    terms <- strsplit(as.character(formula)[3], "\\+")[[1]]
    gsub("^\\s+|\\s+$", "", terms)
}
