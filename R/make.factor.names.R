#' Make a factor name
#'
#' Construct a factor name for each level
#'
#' @param name the factor to be decomposed
#' @param levels data frame with that as a column, or levels to produce
#' @param sep a paste separator for creating factor names
#' @return Vector of the factor names
#' @examples
#' make.factor.names("Species", iris)
#' make.factor.names("Species", unique(iris$Species))

make.factor.names <- function(name, levels, sep="") {
    if (class(levels) == "data.frame")
        make.factor.names(name, unique(levels[, name]), sep=sep)
    else
        paste(name, gsub("[^a-zA-z0-9]", ".", levels), sep=sep)
}
