#' Indirect Effect for Serial Mediation and Moderator on Path A
#'
#' This function runs a serial mediation model with two mediators to calculate
#' the indirect effect, which will be used for bootstrapping
#' the confidence interval of the indirect effect. This function
#' is used in conjunction with the \code{boot} function and is formatted to
#' work as a bootstrapped effect. This mediation model includes a
#' moderator on path A1.
#'
#' @param formula2 The formula for mediation for the a1 path, usually
#' \code{m1 ~ x}. Can also include covariates and will be \code{eq2}
#' if the \code{createformula()} function is used. This effect will
#' also have a moderator for this model.
#' @param formula3 The formula for mediation for the a2 and d21 paths, usually
#' \code{m2 ~ x + m1}. Can also include covariates and will be
#' \code{eq3} if the \code{createformula()} function is used.
#' @param formula4 The formula for mediation for the b1 and b2 paths, usually
#' \code{y ~ x + m1 + m2}. Can also include covariates and will be \code{eq4} if the
#' \code{createformula()} function is used.
#' @param x The column name for x in the data frame.
#' @param m1 The column name for mediator 1 in the data frame.
#' @param m2 The column name for mediator 2 in the data frame.
#' @param mod The column name for the moderator in the data frame.
#' @param data The dataframe where the columns from the formula can be found.
#' @param random This variable is used to denote the data frame will be
#' randomize by row, as part of the \code{boot} library.
#' @keywords mediation, regression, indirect effect
#' @export
#' @examples
#' indirectmed2("Q31 ~ Q151*Q121", "Q41 ~ Q151 + Q31",
#'              "Q11 ~ Q151 + Q31 + Q41", mtcars)
#' @export

indirectmed2_moda <- function(formula2, formula3,
                        formula4, x, m1, m2, mod, data, random) {
  d = data[random, ] #randomize by row

  #figure out x categorical
  if (is.factor(data[ , x])){
    xcat = T
    levelsx = paste(x, levels(data[, x])[-1], sep = "")
    } else { xcat = F }

  #run the models
  model2 = lm(formula2, data = d)
  model2.1 = lm(gsub(mod, "lowMod", formula2), data = d)
  model2.2 = lm(gsub(mod, "highMod", formula2), data = d)
  model3 = lm(formula3, data = d)
  model4 = lm(formula4, data = d)

  if (xcat == F){ #run this with continuous X

    #relevant coefficients
    a1_avg = coef(model2)[x]
    a1_low = coef(model2.1)[x]
    a1_high = coef(model2.2)[x]
    b1 = coef(model4)[m1]
    a2 = coef(model3)[x]
    b2 = coef(model4)[m2]
    d21 = coef(model3)[m1]

    #reporting
    indirect1_avg = a1_avg*b1
    indirect1_low = a1_low*b1
    indirect1_high = a1_high*b1
    indirect2 = a2*b2
    indirect3_avg = a1_avg*d21*b2
    indirect3_low = a1_low*d21*b2
    indirect3_high = a1_high*d21*b2

  } else {

    #figure out all the labels for X
    levelsx = paste(x, levels(df[, x])[-1], sep = "")
    indirect1_avg = NA; indirect1_low = NA; indirect1_high = NA;
    indirect2 = NA; indirect3_avg = NA; indirect3_low = NA; indirect3_high = NA

    #loop over that to figure out sobel and reporting
    for (i in 1:length(levelsx)){

      #relevant coefficients
      a1_avg = coef(model2)[levelsx[i]]
      a1_low = coef(model2.1)[levelsx[i]]
      a1_high = coef(model2.2)[levelsx[i]]
      b1 = coef(model4)[m1]
      a2 = coef(model3)[levelsx[i]]
      b2 = coef(model4)[m2]
      d21 = coef(model3)[m1]

      #reporting
      indirect1_avg[i] = a1_avg*b1
      indirect1_low[i] = a1_low*b1
      indirect1_high[i] = a1_high*b1
      indirect2[i] = a2*b2
      indirect3_avg[i] = a1_avg*d21*b2
      indirect3_low[i] = a1_low*d21*b2
      indirect3_high[i] = a1_high*d21*b2

    } #close for loop
  } #close else x is categorical

  return(c(indirect1_avg, indirect1_low, indirect1_high,
           indirect2, indirect3_avg, indirect3_low, indirect3_high))
}

#' @rdname indirectmed2_moda
#' @export
