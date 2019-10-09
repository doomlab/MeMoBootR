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
#' @param m1 The column name for mediator 1 in the data frame.
#' @param m2 The column name for mediator 2 in the data frame.
#' @param data The dataframe where the columns from the formula can be found.
#' @param random This variable is used to denote the data frame will be
#' randomize by row, as part of the \code{boot} library.
#' @keywords mediation, regression, indirect effect
#' @export
#' @examples
#' indirectmed2("Q31 ~ Q151", "Q41 ~ Q151 + Q31",
#'              "Q11 ~ Q151 + Q31 + Q41", mtcars)
#' @export

indirectmed2 = function(formula2, formula3,
                        formula4, x, m1, m2, df, random) {
  d = df[random, ] #randomize by row

  #figure out x categorical
  if (is.factor(df[ , x])){
    xcat = T
    levelsx = paste(x, levels(df[, x])[-1], sep = "")
    } else { xcat = F }

  #run the models
  model2 = lm(formula2, data = d)
  model3 = lm(formula3, data = d)
  model4 = lm(formula4, data = d)

  if (xcat == F) { #run this if X is continuous

    #relevant coefficients
    a1 = coef(model2)[x]
    b1 = coef(model4)[m1]
    a2 = coef(model3)[x]
    b2 = coef(model4)[m2]
    d21 = coef(model3)[m1]

    indirect1 = a1*b1
    indirect2 = a2*b2
    indirect3 = a1*d21*b2

  } else {
    indirect = NA;indirect2 = NA; indirect3 = NA
    for (i in 1:length(levelsx)) {
      #relevant coefficients
      a1 = coef(model2)[levelsx[i]]
      b1 = coef(model4)[m1]
      a2 = coef(model3)[levelsx[i]]
      b2 = coef(model4)[m2]
      d21 = coef(model3)[m1]

      indirect1[i] = a1*b1
      indirect2[i] = a2*b2
      indirect3[i] = a1*d21*b2

    } #close for loop around x
  } #close else statement

  return(c(indirect1, indirect2, indirect3))
}

#' @rdname indirectmed2
#' @export
