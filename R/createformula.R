#' Create Regression Formulas
#'
#' This function creates the formulas for \code{lm()} to run the
#' regression models necessary for data screening and mediation
#' or moderation models.
#'
#' The output currently includes a list of equations used for simple mediation,
#' which will updated as moderation is added to the package.
#'
#' @param y The dependent variable column name from your dataframe.
#' @param x The independent variable column name from your dataframe. This column will be treated as X in mediation or moderation models, please see diagrams online for examples.
#' @param m The first mediator or moderator for your model.
#' @param m2 The second mediator or moderator for your model.
#' @param mod A moderator for a moderated mediation model.
#' @param cvs The covariates you would like to include in the model. Use a `c()` concatenated vector to use multiple covariates.
#' @keywords mediation, moderation, regression, formulas
#' @export
#' @examples
#' createformula(y = "cyl", x = "mpg", m = "disp",
#'              cvs = c("drat", "gear"), type = "moderation1")
#' @export

createformula = function (y, x, m, m2 = NULL, mod = NULL, cvs = NULL, type){

  if (type == "mediation1") {

    if (!is.null(cvs)) {
      #y ~ x + cvs
      eq1 = paste(y, "~", x, "+", paste(cvs, collapse = " + "), sep = " ")
      #m ~ x + cvs
      eq2 = paste(m, "~", x, "+", paste(cvs, collapse = " + "), sep = " ")
      #y ~ x + m + cvs
      eq3 = paste(y, "~", x, "+", m, "+", paste(cvs, collapse = " + "), sep = " ")
    } else {
      #y ~ x
      eq1 = paste(y, "~", x, sep = " ")
      #m ~ x
      eq2 = paste(m, "~", x, sep = " ")
      #y ~ x
      eq3 = paste(y, "~", x, "+", m, sep = " ")
    }
    return(list("eq1" = eq1,
                "eq2" = eq2,
                "eq3" = eq3))

  } #return simple mediation

  if (type == "mediation2") {

    if (!is.null(cvs)) {
      #y ~ x + cvs
      eq1 = paste(y, "~", x, "+", paste(cvs, collapse = " + "), sep = " ")
      #m1 ~ x + cvs
      eq2 = paste(m, "~", x, "+", paste(cvs, collapse = " + "), sep = " ")
      #m2 ~ x + m1 + cvs
      eq3 = paste(m2, "~", x, "+", m, "+", paste(cvs, collapse = " + "), sep = " ")
      #y ~ x + m + cvs
      eq4 = paste(y, "~", x, "+", m, "+", m2, "+", paste(cvs, collapse = " + "), sep = " ")
    } else {
      #y ~ x
      eq1 = paste(y, "~", x, sep = " ")
      #m1 ~ x + cvs
      eq2 = paste(m, "~", x, sep = " ")
      #m2 ~ x + m1 + cvs
      eq3 = paste(m2, "~", x, "+", m, sep = " ")
      #y ~ x + m + cvs
      eq4 = paste(y, "~", x, "+", m, "+", m2, sep = " ")
    }
    return(list("eq1" = eq1,
                "eq2" = eq2,
                "eq3" = eq3,
                "eq4" = eq4))

  } #return serial mediation with two mediators

  if (type == "mediation2_moda") {

    if (!is.null(cvs)) {
      #y ~ x + cvs
      eq1 = paste(y, "~", x, "+", paste(cvs, collapse = " + "), sep = " ")
      #m1 ~ x + cvs
      eq2 = paste(m, "~", x, "*", mod, "+", paste(cvs, collapse = " + "), sep = " ")
      eq2.1 = paste(m, "~", x, "*", "lowMod", "+", paste(cvs, collapse = " + "), sep = " ")
      eq2.2 = paste(m, "~", x, "*", "highMod", "+", paste(cvs, collapse = " + "), sep = " ")
      #m2 ~ x + m1 + cvs
      eq3 = paste(m2, "~", x, "+", m, "+", paste(cvs, collapse = " + "), sep = " ")
      #y ~ x + m + cvs
      eq4 = paste(y, "~", x, "+", m, "+", m2, "+", paste(cvs, collapse = " + "), sep = " ")
    } else {
      #y ~ x
      eq1 = paste(y, "~", x, sep = " ")
      #m1 ~ x + cvs
      eq2 = paste(m, "~", x, "*", mod, sep = " ")
      eq2.1 = paste(m, "~", x, "*", "lowMod", sep = " ")
      eq2.2 = paste(m, "~", x, "*", "highMod", sep = " ")
      #m2 ~ x + m1 + cvs
      eq3 = paste(m2, "~", x, "+", m, sep = " ")
      #y ~ x + m + cvs
      eq4 = paste(y, "~", x, "+", m, "+", m2, sep = " ")
    }
    return(list("eq1" = eq1,
                "eq2" = eq2,
                "eq2.1" = eq2.1,
                "eq2.2" = eq2.2,
                "eq3" = eq3,
                "eq4" = eq4))

  } #return serial mediation with two mediators

  if (type == "moderation1"){

    if (!is.null(cvs)) {
      #y ~ x * m + cvs
      eq1 = paste(y, "~", x, "*", m, "+", paste(cvs, collapse = " + "), sep = " ")
      eq2 = paste(y, "~", x, "*", "lowM", "+", paste(cvs, collapse = " + "), sep = " ")
      eq3 = paste(y, "~", x, "*", "highM", "+", paste(cvs, collapse = " + "), sep = " ")
    } else {
      #y ~ x * m
      eq1 = paste(y, "~", x, "*", m, sep = " ")
      eq2 = paste(y, "~", x, "*", "lowM", sep = " ")
      eq3 = paste(y, "~", x, "*", "highM", sep = " ")
    }
    return(list("eq1" = eq1,
                "eq2" = eq2,
                "eq3" = eq3))

  } #return simple moderation

  if (type == "moderation2"){

    if (!is.null(cvs)) {
      #y ~ x * m + cvs overallmodel
      eq1 = paste(y, "~", x, "*", m, "+", x, "*", m2, "+",
                  paste(cvs, collapse = " + "), sep = " ")
      #avg m1 low m2
      eq1.1 = paste(y, "~", x, "*", m, "+", x, "*", "lowM2", "+",
                    paste(cvs, collapse = " + "), sep = " ")
      #avg m1 high m2
      eq1.2 = paste(y, "~", x, "*", m, "+", x, "*", "highM2", "+",
                    paste(cvs, collapse = " + "), sep = " ")
      #low m1 avg m2
      eq2 = paste(y, "~", x, "*", "lowM1", "+", x, "*", m2, "+",
                  paste(cvs, collapse = " + "), sep = " ")
      #low m1 low m2
      eq2.1 = paste(y, "~", x, "*", "lowM1", "+", x, "*", "lowM2", "+",
                    paste(cvs, collapse = " + "), sep = " ")
      #low m1 high m2
      eq2.2 = paste(y, "~", x, "*", "lowM1", "+", x, "*", "highM2", "+",
                    paste(cvs, collapse = " + "), sep = " ")
      #high m1 avg m2
      eq3 = paste(y, "~", x, "*", "highM1", "+", x, "*", m2, "+",
                  paste(cvs, collapse = " + "), sep = " ")
      #high m1 low m2
      eq3.1 = paste(y, "~", x, "*", "highM1", "+", x, "*", "lowM2", "+",
                    paste(cvs, collapse = " + "), sep = " ")
      #high m1 high m2
      eq3.2 = paste(y, "~", x, "*", "highM1", "+", x, "*", "highM2", "+",
                    paste(cvs, collapse = " + "), sep = " ")
    } else {
      #y ~ x * m overallmodel
      eq1 = paste(y, "~", x, "*", m, "+", x, "*", m2, sep = " ")
      #avg m1 low m2
      eq1.1 = paste(y, "~", x, "*", m, "+", x, "*", "lowM2", sep = " ")
      #avg m1 high m2
      eq1.2 = paste(y, "~", x, "*", m, "+", x, "*", "highM2", sep = " ")
      #low m1 avg m2
      eq2 = paste(y, "~", x, "*", "lowM1", "+", x, "*", m2, sep = " ")
      #low m1 low m2
      eq2.1 = paste(y, "~", x, "*", "lowM1", "+", x, "*", "lowM2", sep = " ")
      #low m1 high m2
      eq2.2 = paste(y, "~", x, "*", "lowM1", "+", x, "*", "highM2", sep = " ")
      #high m1 avg m2
      eq3 = paste(y, "~", x, "*", "highM1", "+", x, "*", m2, sep = " ")
      #high m1 low m2
      eq3.1 = paste(y, "~", x, "*", "highM1", "+", x, "*", "lowM2", sep = " ")
      #high m1 high m2
      eq3.2 = paste(y, "~", x, "*", "highM1", "+", x, "*", "highM2", sep = " ")
    }
    return(list("eq1" = eq1,
                "eq2" = eq2,
                "eq3" = eq3,
                "eq1.1" = eq1.1,
                "eq1.2" = eq1.2,
                "eq2.1" = eq2.1,
                "eq2.2" = eq2.2,
                "eq3.1" = eq3.1,
                "eq3.2" = eq3.2))

  } #return two two-way moderation

}

#' @rdname createformula
#' @export
