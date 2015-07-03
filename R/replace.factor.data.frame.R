#' Replace a factor column
#'
#' Constructs indicator columns for a factor variable.
#'
#' @param name column name
#' @param data data frame
#' @param iftrue value to enter in column for the indicator
#' @param sep paste sep used to construct factor names
#' @return Modified data frame
#' @examples
#' replace.factor.data.frame("Species", data=iris, 1)

replace.factor.data.frame <- function(name, data, iftrue, sep="") {
    ## Add factor level columns
    for (factor in make.factor.names(name, data, sep=sep))
        data[, factor] <- ifelse(data[, name] == level, iftrue, 0)
    ## Remove original factor column
    data[, -which(names(data) == name)]
}

