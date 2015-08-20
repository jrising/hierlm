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
    levels <- unique(data[, name])
    levelnames <- make.factor.names(name, levels, sep=sep)
    for (ii in 1:length(levels))
        data[, levelnames[ii]] <- ifelse(data[, name] == levels[ii], iftrue, 0)
    ## Remove original factor column
    data[, -which(names(data) == name)]
}

