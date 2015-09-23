#' Add rows for priors on coefficients
#'
#' Row values scaled so that errors like sde match the given standard
#' error.
#' Calculate sde by estimating equation without the priors, and then calling
#' sde <- sd(noprior.fit$residuals)
#'
#' Sets up equations of the form
#' mean / scale = beta * 1 / scale + epsilon
#'
#' @param data data frame
#' @param priors data frame of coeff, mean, serr, weight
#' @param depvar character, the name of the dependent variable
#' @param sde numeric, the sd of residuals without the priors
#' @return Augmented data frame
#' @examples
#' mod <- lm(Sepal.Length ~ Sepal.Width, data=iris)
#' sde <- sd(mod$residuals)
#' iris2 <- add.priors(iris, data.frame(coeff="Sepal.Width", mean=0, serr=.1), "Sepal.Length", sde)
#' summary(lm(Sepal.Length ~ Sepal.Width, data=iris2))

add.priors <- function(data, priors, depvar, sde) {
    scales <- sde / priors$serr

    result <- data.frame(ignore=rep(0, nrow(priors)))
    result[, depvar] <- priors$mean * scales * priors$weight
    for (ii in 1:nrow(priors)) {
        column <- as.character(priors$coeff[ii])
        result[, column] <- 0
        result[ii, column] <- scales[ii] * priors$weight[ii]
    }

    rbind.fill.data.frame(data, result)
}

