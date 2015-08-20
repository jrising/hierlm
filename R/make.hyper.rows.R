#' Create fictional observations for a hyper relationship
#'
#' Return a data frame with fictional observations
#'
#' @param nonhyper name of the non-hyper column
#' @param hyper name of the hyper column
#' @param weight weight to be used in the rows
#' @param data a data frame for inspecting levels
#' @return Fictional observations in a data frame
#' @examples
#' make.hyper.rows("Species", "hyperSpecies", 5, iris)

make.hyper.rows <- function(nonhyper, hyper, weight, data, sep="") {
    if (grepl(':', nonhyper)) {
        subterms <- split.interaction.term(nonhyper)
        for (subterm in subterms) {
            if (!is.numeric(data[, subterm])) {
                extras <- make.hyper.rows(subterm, hyper, weight, data)
                extras[, subterms[subterms != subterm]] <- 1
                ##extra.names <- names(extras)
                ##replacement <- paste("\\1", subterms[subterms != subterm], sep="X")
                ##extra.names <- sub(paste0("(^", subterm, ".*)"), replacement, extra.names)
                ##names(extras) <- extra.names
                return(extras)
            }
        }
    } else if (grepl(':', hyper)) {
        subterms <- split.interaction.term(hyper)
        for (subterm in subterms) {
            if (!is.numeric(data[, subterm])) {
                extras <- make.hyper.rows(nonhyper, subterm, weight, data)
                extras[, subterms[subterms != subterm]] <- 1
                ##extra.names <- names(extras)
                ##replacement <- paste("\\1", subterms[subterms != subterm], sep="X")
                ##extra.names <- sub(paste0("(^", subterm, ".*)"), replacement, extra.names)
                ##names(extras) <- extra.names
                return(extras)
            }
        }
    } else {
        ## Construct extra from diagonal factor matrix
        extras <- diag(weight, length(unique(data[, nonhyper])))
        extras <- as.data.frame(extras)

        levels <- unique(data[, nonhyper])
        names(extras) <- make.factor.names(nonhyper, levels, sep=sep)
        if (hyper %in% names(data) && !is.numeric(data[, hyper])) {
            ## Data should specify the rhs in each lhs case
            hyperlevels <- unique(data[, hyper])
            for (hyperlevel in hyperlevels) {
                superset <- unique(data[data[, hyper] == hyperlevel, nonhyper])
                extras[, make.factor.names(hyper, hyperlevel, sep=sep)] <- ifelse(levels %in% superset, -weight, 0)
            }
        } else {
            ## Add in the hyper column
            extras[, hyper] <- -weight
        }

        extras
    }
}
