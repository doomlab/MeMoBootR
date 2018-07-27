#' Indirect Effect for Serial Mediation
#'
#' This function runs a serial mediation model with two mediators to calculate
#' the indirect effect, which will be used for bootstrapping
#' the confidence interval of the indirect effect. This function
#' is used in conjunction with the \code{boot} function and is formatted to
#' work as a bootstrapped effect.
#'
#' @param formula2 The formula for mediation for the a1 path, usually
#' \code{m1 ~ x}. Can also include covariates and will be \code{eq2}
#' if the \code{createformula()} function is used.
#' @param formula3 The formula for mediation for the a2 and d21 paths, usually
#' \code{m2 ~ x + m1}. Can also include covariates and will be
#' \code{eq3} if the \code{createformula()} function is used.
#' @param formula4 The formula for mediation for the b1 and b2 paths, usually
#' \code{y ~ x + m1 + m2}. Can also include covariates and will be \code{eq4} if the
#' \code{createformula()} function is used.
#' @param x The column name for x in the data frame.
#' @param med.var The column name for m in the data frame.
#' @param df The dataframe where the columns from the formula can be found.
#' @param random This variable is used to denote the data frame will be
#' randomize by row, as part of the \code{boot} library.
#' @keywords mediation, regression, indirect effect
#' @export
#' @examples
#' indirectmed("disp ~ mpg", "cyl ~ mpg + disp", mtcars)
#' @export

indirectmed = function(formula2, formula3, formula4, x, m1, m2, df, random) {
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
  b = coef(model3)[med.var]
  indirect = a*b
  } else {
    indirect = NA
    for (i in 1:length(levelsx)) {
      a = coef(model2)[levelsx[i]]
      b = coef(model3)[med.var]
      indirect[i] = a*b
    } #close for loop around x
  } #close else statement

  return(indirect)
}

#' @rdname indirectmed
#' @export
