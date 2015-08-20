#' Replace a factor term
#'
#' Constructs indicator terms for a factor variable.
#'
#' @param name term name
#' @param formula formula
#' @param data a data frame to be used to identify factor levels
#' @param sep paste sep used to construct factor names
#' @return Modified formula
#' @examples
#' replace.factor.formula("Species", Sepal.Length ~ Species, iris)
#' replace.factor.formula("Species", Sepal.Length ~ Petal.Length : Species, iris)

replace.factor.formula <- function(name, formula, data, sep="") {
    terms <- c()

    for (term in get.formula.terms(formula)) {
        if (term == name) {
            terms <- c(terms, make.factor.names(name, data))
        } else if (grepl(':', term)) {
            subterms <- interaction.term.split(term)
            if (subterms[1] == name) {
                terms <- c(terms, paste(make.factor.names(subterms[1], data), subterms[2], sep=':'))
            } else if (subterms[2] == name) {
                terms <- c(terms, paste(subterms[1], make.factor.names(subterms[2], data), sep=':'))
            } else {
                terms <- c(terms, term)
            }
        } else {
            terms <- c(terms, term)
        }
    }

    ## Remove duplicates
    terms <- terms[!duplicated(terms)]

    chunks <- as.character(formula)
    as.formula(paste(chunks[2], paste(terms, collapse=" + "), sep=" ~ "))
}

