#' Indirect Effect for Simple Mediation
#'
#' This function runs a simple mediation model to calculate
#' the indirect effect, which will be used for bootstrapping
#' the confidence interval of the indirect effect. This function
#' is used in conjunction with the \code{boot} function and is formatted to
#' work as a bootstrapped effect.
#'
#' @param formula2 The formula for mediation for the a path, usually
#' \code{m ~ x}. Can also include covariates and will be \code{eq2}
#' if the \code{createformula()} function is used.
#' @param formula3 The formula for mediation for the b path, usually
#' \code{y ~ x + m}. Can also include covariates and will be
#' \code{eq3} if the \code{createformula()} function is used.
#' @param df The dataframe where the columns from the formula can be found.
#' @param random This variable is used to denote the data frame will be
#' randomize by row, as part of the \code{boot} library.
#' @keywords mediation, moderation, regression, indirect effect
#' @export
#' @examples
#' indirectm1("disp ~ mpg", "cyl ~ mpg + disp", mtcars)
#' @export

indirectm1 = function(formula2, formula3, df, random) {
  d = df[random, ] #randomize by row
  model2 = lm(formula2, data = d)
  model3 = lm(formula3, data = d)
  a = coef(model2)[2]
  b = coef(model3)[3]
  indirect = a*b
  return(indirect)
}

#' @rdname indirectm1
#' @export
