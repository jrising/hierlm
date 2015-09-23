#' Add rows, setting additional columns to 0
#'
#' Any columns in data1 but not in data2 will be set to 0 in the
#' additional rows from data1.
#' Any columns in data2 but not in data1 will be ignored.
#'
#' @param data1 data frame
#' @param data2 dataframe
#' @return Augmented data frame
#' @examples
#' rbind.fill.data.frame(iris, data.frame(Sepal.Width=5))

rbind.fill.data.frame <- function(data1, data2) {
    for (column in names(data1))
        if (!(column %in% names(data2)))
            data2[, column] <- 0

    rbind(data1, data2[, names(data1)])
}

