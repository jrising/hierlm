#' Make a factor name
#'
#' Construct a factor name for each level
#'
#' @param name the factor to be decomposed
#' @param data data frame with that as a column
#' @return Vector of the factor names
#' @examples
#' make.factor.names("Species", iris)

make.factor.names <- function(name, data, sep="") {
    paste(name, unique(data[, name]), sep=sep)
}
