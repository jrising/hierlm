#' Fit an hierarchical model
#'
#' Interpret a hierarchical formula and fit it with the given data and weight ratios.
#'
#' @param formula an hierlm-style formula
#' @param datas a data frame or a list of data frames, for each lm-style formula contained in the hierlm
#' @param ratios a vector a ratios for the effect of fictional
#'   observations, with length as large as the number of hyper
#'   expressions.
#' @return A fitted lm model
#' @examples
#' hierlm(Sepal.Length ~ 0 + Petal.Length + Species + Sepal.Width : Species | Species - ., iris, 1)
#' hierlm(Sepal.Length ~ 0 + Petal.Length + Species + Sepal.Width : Species | Species - . | Sepal.Width : Species - ., iris, c(1, 2))

hierlm <- function(formula, datas, ratios, sep="") {
    if (is.data.frame(datas))
        datas <- list(datas)

    data.ii <- 1
    ratio.ii <- 1

    data <- datas[[data.ii]]

    ## Split the formula by |
    labels <- attr(terms(formula), 'term.labels')
    if (length(labels) == 1) { # otherwise it's just an lm formula
        lhs <- as.character(formula)[2]
        chunks <- strsplit(labels, "\\|")[[1]]
        formula <- as.formula(paste(lhs, "~", chunks[1]), env=attr(formula, '.Environment'))
        ## Keep this for weight calculations
        oldformula <- formula
        for (chunk in chunks[-1]) {
            ## Make the new formula
            formula <- combine.formulae(formula, chunk)
            if (!("constant" %in% names(data)) && "constant" %in% get.formula.terms(formula))
                data$constant <- 1

            terms <- get.hyper.terms(chunk)
            if (length(terms) == 3) {
                ## Build the new relationship
                print(oldformula)
                print(c(terms[1], ratios[ratio.ii]))
                weight <- calc.fo.weights(oldformula, terms[1], ratios[ratio.ii], datas[[data.ii]]) # use original data
                print(weight)
                extras <- make.hyper.rows(terms[1], terms[2], weight, datas[[data.ii]])

                ## Done with this ratio
                ratio.ii <- ratio.ii + 1

                ## Create indicators for nonhyper, if not already done
                if (terms[1] %in% data)
                    data <- replace.factor(terms[1], data, 1, sep=sep)

                data <- combine.data.frames(data, extras)
            } else {
                ## Add in the next formula
                data.ii <- data.ii + 1
                data <- combine.data.frame(data, datas[[data.ii]])
            }

            ## Have any of the terms in the formula been factorized?
            checks <- get.formula.terms(formula)
            for (check in checks) {
                if (grepl(':', check)) {
                    for (subterm in split.interaction.term(check))
                        if (subterm %in% names(datas[[data.ii]]) && !(subterm %in% names(data)) && !is.numeric(datas[[data.ii]][, subterm]))
                            formula <- replace.factor.formula(subterm, formula, datas[[data.ii]])
                } else {
                    if (check %in% names(datas[[data.ii]]) && !(check %in% names(data)) && !is.numeric(datas[[data.ii]][, check]))
                        formula <- replace.factor.formula(check, formula, datas[[data.ii]])
                }
            }
        }
    }

    print(head(data))
    print(tail(data))
    print(formula)
    lm(formula, data=data)
}

