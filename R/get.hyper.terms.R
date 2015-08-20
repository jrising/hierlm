#' Get any new hyper terms from formula
#'
#' If the "a - b" formula requires the creation of a new hyper terms
#' (one of a or b is .), return them.
#' Can also be of the form "a > b"
#'
#' @param formula Single component of a hierlm formula
#' @param sep a paste separator for creating hyper terms
#' @return Vector of c(nonhyper, hyper, is.interaction), if a new one was created;
#'   or c(left, right, NA) if both terms are given;
#'   or c() if not an "a - b" expression
#' @examples
#' get.hyper.terms(Sepal.Length ~ Petal.Length + Species + Sepal.Width : Species)

get.hyper.terms <- function(formula, sep="") {
    if (class(formula) == "formula")
        return(c())

    terms <- strsplit(formula, "-")[[1]]
    relation.type <- "-"
    if (length(terms) != 2) {
        terms <- strsplit(formula, ">")[[1]]
        relation.type <- ">"
        if (length(terms) != 2) {
            terms <- strsplit(formula, "==")[[1]]
            relation.type <- "=="
            if (length(terms) != 2)
                return(c())
        }
    }

    terms <- gsub("^\\s+|\\s+$", "", terms)
    if ("." %in% terms) {
        nonhyper <- terms[terms != "."]
        if (grepl(':', nonhyper)) {
            subterms <- interaction.term.split(nonhyper)
            hyper <- paste("hyper", paste(subterms, collapse=sep), sep=sep)
            return(c(nonhyper, hyper, T))
        } else {
            hyper <- paste("hyper", nonhyper, sep=sep)
            return(c(nonhyper, hyper, F))
        }
    }
    return(c(terms, relation.type))
}
