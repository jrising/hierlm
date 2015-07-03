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
        for (subterm in split.interaction.term(nonhyper)) {
            if (!is.numeric(data[, subterm]))
                return(make.hyper.rows(subterm, hyper, weight, data))
        }
    } else {
        ## Construct extra from diagonal factor matrix
        extras <- diag(weight, length(unique(data[, nonhyper])))
        extras <- as.data.frame(extras)

        names(extras) <- paste(nonhyper, unique(data[, nonhyper]), sep=sep)

        ## Add in the hyper column
        extras[, hyper] <- -weight

        extras
    }
}
