#' transform difference in survival rate
#' between treatment of interest and baseline treatment(in %) to log HR scale
#' In the example it is used to transform clinician's opinions to
#' the parametre of interest - log HR
#'
#' Example is given in the paper Parmar (1994) and partly in
#' Spiegelhalter (2004), chapter 5, example 5.1
#'
#' @param x values you want to transform in %, difference in survival rate
#' @param baseline bl - baseline
#'
#' @return value x on a log HR scale
#'
#' @examples
#' pooled <- c(rep(-7.5, 3), rep(-2.5, 7), rep(2.5, 20), rep(7.5, 21),
#'           rep(12.5, 25), rep(17.5, 18), rep(22.5, 5), 27.5)
#' surv_diff_to_logHR(pooled, 15)
#'
#' @author Natalia Popova
#'
#' @export
surv_diff_to_logHR <- function (x, baseline) {
  out <- log(log(x / 100 + baseline / 100) / log(baseline / 100))
  return(out)
}




