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
#' @param m The first mediator or moderator for your model. This function will incorporate multiple mediators/moderators later.
#' @param cvs The covariates you would like to include in the model. Use a `c()` concatenated vector to use multiple covariates.
#' @keywords mediation, moderation, regression, formulas
#' @export
#' @examples
#' createformula(y = "cyl", x = "mpg", m = "disp",
#'              cvs = c("drat", "gear"))
#' @export

createformula = function (y, x, m, cvs = NULL){

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
}

#' @rdname createformula
#' @export
