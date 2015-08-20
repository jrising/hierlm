#' Extract the terms from an lm formula
#'
#' Return a list of terms in an lm formula, in the order they appear
#'
#' @param formula lm-style formula
#' @param all.names boolean to extract every unique name
#' @return Vector of the terms in the lm formula
#' @examples
#' get.formula.terms(Sepal.Length ~ Petal.Length + Species + Sepal.Width : Species)

get.formula.terms <- function(formula, all.names=F) {
    if (all.names)
        return(unique(strsplit(gsub("[^a-zA-Z0-9]", " ", paste(as.character(formula)[2:3], collapse="~")), "\\s+")[[1]]))

    terms <- strsplit(as.character(formula)[3], "\\+")[[1]]
    gsub("^\\s+|\\s+$", "", terms)
}
