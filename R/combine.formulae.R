#' Combine two formulas in hierlm-format
#'
#' Return the left and right side of a new combined formula.
#' Left formula must be a standard formula.  Right formula may take the following forms:
#' y ~ ... : an lm-style formula; the left-hand-side variables much match
#' f - b : A requirement that two variables be close together (typically one is a factor)
#' f - . : A requirement that a variable (or set of factors) be close to a new hyper-variable
#' a : f - [b or .] : A requirement that the results of an interaction be near a hyper-variable
#' TODO:
#' a + B : A requirement that the factors in 'a' are similar to eachother by smoothness 'B'
#'
#' @param formula1 an lm-style formula
#' @param formula2 an additional formula or hierlm term
#' @return Augmented formula
#' @examples
#' combine.formulae(combine.formulae(combine.formulae(y ~ a + b, "b - c"), y ~ c + d), "c : d - .")

combine.formulae <- function(formula1, formula2, sep="") {
    chunks1 <- as.character(formula1)

    if (is.character(formula2))
        if (grepl("~", formula2))
            formula2 <- as.formula(formula2)

    if (class(formula2) == "formula") {
        chunks2 <- as.character(formula2)
        if (chunks2[2] != chunks1[2])
            error("Left-hand-side variables do not match.")

        terms1 <- get.formula.terms(formula1)
        terms2 <- get.formula.terms(formula2)
    } else {
        terms2 <- get.hyper.terms(formula2, sep=sep)
        if (length(terms2) != 3)
            error(paste("Cannot understand expression:", formula2))

        terms2 <- terms2[-3]

        terms1 <- get.formula.terms(formula1)
        if (attr(terms(formula1), 'intercept') == 0) {
            if (!("0" %in% terms1))
                terms1 <- c("0", terms1)
        } else {
            if (!("0" %in% terms1 && "constant" %in% terms1))
                terms1 <- c("0", "constant", terms1)
        }
    }

    for (term in terms2)
        if (!(term %in% terms1))
            terms1 <- c(terms1, term)

    as.formula(paste(chunks1[2], "~", paste(terms1, collapse=" + ")),
               env=attr(formula1, '.Environment'))
}

