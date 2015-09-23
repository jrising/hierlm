#' Add additional columns to a dataframe, as zeros
#'
#' @param data data frame
#' @param columns vector of characters
#' @return Augmented data frame
#' @examples
#' cbind.fill.data.frame(iris, c('one', 'two'))

cbind.fill.data.frame <- function(data, columns) {
    for (column in columns)
        data[, column] <- 0

    data
}

