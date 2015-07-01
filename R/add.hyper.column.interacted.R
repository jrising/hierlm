#' Add a Hyper Column
#'
#' Constructs both the column to describe a hyper variable, and the
#' fictional rows to to estimate it.
#'
#' @param column string name of the category to hyper-ize
#' @param interacted string name of interacted column
#' @param weight weight to use for fictional rows
#' @param data data frame
#' @return Augmented data frame
#' @examples
#' add.hyper.column.interacted("Sepal.Width", "Species", 5, iris)

add.hyper.column.interacted <- function(column, factor, weight, data, sep="") {
    hyper <- paste("hyper", column, sep)
    data[, hyper] <- 0

    ## Construct extra rows
    for (level in levels(data[, factor])) {
        extra <- data[1, ]
        for (name in names(extra)) {
            if (name == factor)
                extra[, name] <- level
            else if (name == column)
                extra[, name] <- weight
            else if (name == hyper)
                extra[, name] <- -weight
            else if (is.numeric(extra[, name]))
                extra[, name] <- 0
        }

        data <- rbind(data, extra)
    }

    data
}
