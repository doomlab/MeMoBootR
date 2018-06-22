#' Indirect Effect for Mediation
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
#' indirectmed("disp ~ mpg", "cyl ~ mpg + disp", mtcars)
#' @export

indirectmed = function(formula2, formula3, x, df, random) {
  d = df[random, ] #randomize by row

  #figure out x categorical
  if (is.factor(df[ , x])){
    xcat = T
    levelsx = paste(x, levels(df[, x])[-1], sep = "")
    } else { xcat = F }

  #run the models
  model2 = lm(formula2, data = d)
  model3 = lm(formula3, data = d)

  if (xcat == F) { #run this if X is continuous
  a = coef(model2)[x]
  b = coef(model3)[x]
  indirect = a*b
  } else {
    indirect = NA
    for (i in 1:length(levelsx)) {
      a = coef(model2)[levelsx[i]]
      b = coef(model3)[levelsx[i]]
      indirect[i] = a*b
    } #close for loop around x
  } #close else statement

  return(indirect)
}

#' @rdname indirectmed
#' @export
