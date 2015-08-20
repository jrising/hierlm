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
#' hierlm(Sepal.Length ~ 0 + Petal.Length + Species + Sepal.Width : Species | Sepal.Width : Species - ., iris, 2)

hierlm <- function(formula, datas, ratios=NA, weights=NA, return="fit", verbose="none", sep="") {
    if (!(verbose %in% c("none", "weight", "extras", "final")))
        warning("Argument for verbose not supported.")

    if (class(weights) == "logical")
        weights <- 1 / ratios

    if (is.data.frame(datas))
        datas <- list(datas)

    data.ii <- 1
    weight.ii <- 1

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
            if (!("constant" %in% names(data)) && "constant" %in% get.formula.terms(formula, all.names=T))
                data$constant <- 1

            terms <- get.hyper.terms(chunk)
            if (length(terms) == 3) {
                ## Build the new relationship
                if (terms[3] == "==") {
                    extras <- data.frame(left=weights[weight.ii], right=-weights[weight.ii])
                    extra.names <- terms[1:2]
                    if (grepl(':', terms[1])) {
                        parts <- split.interaction.term(terms[1])
                        if (!is.numeric(data[, parts[1]]) && !is.numeric(data[, parts[2]]))
                            warning("Double factors in interactions with == not supported.")
                        else if (is.numeric(data[, parts[1]]) && is.numeric(data[, parts[2]])) {
                            warning("Assuming left term of == lhs interaction is a broken factor.")
                            extras[, parts[2]] <- 1
                            extra.names <- c(parts[1], extra.names[2], parts[2])
                        } else if (is.numeric(data[, parts[1]])) {
                            extras[, parts[1]] <- 1
                            extra.names <- c(parts[2], extra.names[2], parts[1])
                        } else if (is.numeric(data[, parts[2]])) {
                            extras[, parts[2]] <- 1
                            extra.names <- c(parts[1], extra.names[2], parts[2])
                        }
                    }
                    if (grepl(':', terms[2])) {
                        if (length(extra.names) != 3) {
                            warning("Interaction on only rhs of == not supported.")
                        } else {
                            parts <- split.interaction.term(terms[2])
                            if (extra.names[3] %in% parts)
                                extra.names[2] <- parts[parts != extra.names[3]]
                            else
                                warning("Interaction on rhs of == with something other than the term on lhs not supported.")
                        }
                    }
                    names(extras) <- extra.names
                } else {
                    if (class(ratios) != "logical") {
                        weight <- calc.fo.weights(oldformula, terms[1], ratios[weight.ii], datas[[data.ii]]) # use original data
                        if (verbose == "weight") {
                            print("WEIGHT:")
                            print(oldformula)
                            print(terms[1])
                            print(weight)
                        }
                    } else {
                        weight <- weights[weight.ii]
                    }

                    extras <- make.hyper.rows(terms[1], terms[2], weight, datas[[data.ii]])
                    if (verbose == "extras") {
                        print("EXTRAS:")
                        print(terms[1])
                        print(terms[2])
                        print(extras)
                    }
                }

                ## Done with this ratio
                weight.ii <- weight.ii + 1

                ## Create indicators for nonhyper, if not already done
                if (terms[1] %in% data)
                    data <- replace.factor.data.frame(terms[1], data, 1, sep=sep)
                if (terms[2] %in% data)
                    data <- replace.factor.data.frame(terms[2], data, 1, sep=sep)

                ## Get list of all names in existing formula, in case turned into factors
                ## Any terms not in the formula should be set to 0 when split out.
                keepfactors <- get.formula.terms(oldformula, all.names=T)
                data <- combine.data.frames(data, extras, keepfactors=keepfactors)
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
                    if (check %in% names(datas[[data.ii]]) && !(check %in% names(data)) && !is.numeric(datas[[data.ii]][, check])) {
                        formula <- replace.factor.formula(check, formula, datas[[data.ii]])
                    }
                }
            }
        }
    }

    if (return == "data")
        return(data)
    if (return == "formula")
        return(formula)

    if (verbose == "final") {
        print(head(data))
        print(tail(data))
        print(formula)
    }
    lm(formula, data=data)
}

