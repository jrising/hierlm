#' Combine two data frames
#'
#' Add rows as necessary to construct a dataframe with observations
#' from both of the source data frames.
#'
#' Translates factors into multiple columns as needed.
#'
#' @param data1 data frame
#' @param data2 data frame
#' @param sep a paste separator for creating factor names
#' @param keepfactors a list of factor names to translate (or NULL for
#'   all); excluded factors will be given columns set to 0
#' @return Augmented data frame
#' @examples
#' combine.data.frames(iris, data.frame(Sepal.Length=1, Matched.Sepal.Length=-1))
#' combine.data.frames(iris, data.frame(Species="something else"))

combine.data.frames <- function(data1, data2, sep="", keepfactors=NULL) {
    data <- data1

    if (is.null(keepfactors))
        keepfactors <- names(data1)

    ## Translate factors not also in data2 into multiple columns
    ## Also, translate factors into character (in case factors don't match)
    for (jj in 1:length(data1)) {
        name = names(data1)[jj]
        if (!is.numeric(data1[, name])) {
            if (!(name %in% names(data2))) {
                fillin <- ifelse(name %in% keepfactors, 1, 0)
                data <- replace.factor.data.frame(name, data, fillin, sep=sep)
            } else {
                data[, jj] <- as.character(data[, jj])
            }
        }
    }

    ## Add columns from data2 to data1
    for (name in names(data2)) {
        if (!(name %in% names(data)))
            if (!is.numeric(data2[, name])) {
                ## Add factor level columns
                for (factor in make.factor.names(name, data2, sep=sep))
                    data[, factor] <- 0
            } else {
                data[, name] <- 0
            }
    }

    ## Generate rows of data2 in the form of data
    for (ii in 1:nrow(data2)) {
        row <- data[1, ]
        for (name in names(data)) {
            if (name %in% names(data2)) {
                if (is.numeric(data2[, name])) {
                    row[1, name] <- data2[ii, name]
                } else {
                    row[1, name] <- as.character(data2[ii, name])
                }
            } else {
                row[1, name] <- 0
            }
        }

        data <- rbind(data, row)
    }

    data
}

