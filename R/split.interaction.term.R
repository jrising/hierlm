#' Split an Interaction Term
#'
#' Extract the two pieces of an interaction term
#'
#' @param term An interaction (a:b) term
#' @return Vector of the subterms (c(a, b))
#' @examples
#' split.interaction.term("Petal.Length : Species")

split.interaction.term <- function(term) {
    subterms <- strsplit(term, ":")[[1]]
    gsub("^\\s+|\\s+$", "", subterms)
}
