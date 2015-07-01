#' Add a Hyper Column
#'
#' Constructs both the column to describe a hyper variable, and the
#' fictional rows to to estimate it.
#'
#' @param column string name of the category to hyper-ize
#' @param weight weight to use for fictional rows
#' @param data data frame
#' @return Augmented data frame.  Also creates the factor column, so do not include with add.factor.columns.
#' @examples
#' add.hyper.column.factor("Species", 5, data=iris)

add.hyper.column.factor <- function(column, weight, data, sep="") {
    hyper <- paste("hyper", column, sep)
    data[, hyper] <- 0

    ## Construct extra rows
    extras <- data.frame()
    for (level in levels(data[, column])) {
        extra <- data[1, ]
        for (name in names(extra)) {
            if (name == hyper)
                extra[, name] <- -weight
            else if (is.numeric(extra[, name]))
                extra[, name] <- 0
        }
        extras <- rbind(extras, extra)
    }

    column.formula <- as.formula(paste(". ~", column))
    extras <- add.factor.columns(column.formula, extras, iftrue=weight)
    data <- add.factor.columns(column.formula, data, iftrue=0)

    rbind(data, extras)
}

