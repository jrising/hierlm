#' Add Factor Columns
#'
#' Constructs indicator columns for a factor variable.
#'
#' @param formula lm-style formula
#' @param data data frame
#' @param iftrue value to enter in column for the indicator
#' @param sep paste sep used to construct factor names
#' @return Augmented data frame
#' @examples
#' add.factor.columns("Species", data=iris)

add.factor.columns <- function(formula, data, iftrue, sep="") {
    data$constant <- iftrue

    for (term in attr(terms(formula), 'term.labels')) {
        if (term %in% names(data) && is.factor(data[, term]))
            for (level in levels(data[, column]))
                data[, paste(column, level, sep)] <- ifelse(data[, column] == level, iftrue, 0)

    data
}

