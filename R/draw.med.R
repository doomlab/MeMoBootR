#' Simple Mediation Diagrams
#'
#' This function returns the code to create a simple mediation triangle,
#' as well as the basic diagram using the \code{diagram} library.
#'
#' @param model1 The saved regression model of \code{y~x} model where X predicts Y.
#' @param model2 The saved regression model of \code{m~x} model where X predicts M.
#' @param model3 The saved regression model of \code{y~x+m} model where X and M predict Y.
#' @param y The dependent variable from your mediation.
#' @param x The x variable from your mediation.
#' @param m The mediator from your mediation.
#' @param df The dataframe containing the y, x, m columns.
#' @keywords mediation, data screening, bootstrapping, diagram
#' @export
#' @examples
#' draw.med(saved$model1, saved$model2, saved$model3)
#' Note in this example saved is the name of the model saved from mediation1.
#' You can include any form of model names that you have saved from \code{lm()}.
#' You can also type \code{draw.med} in your console to get this code and edit
#' the diagram parameters to your liking.
#' @export

draw.med <- function(model1, model2, model3, y, x, m, df) {

  require(diagram)
  #figure out x categorical
  if (is.factor(df[ , x])){
    xcat = T
    levelsx = paste(x, levels(df[, x])[-1], sep = "")
  } else {xcat = F}

  if (xcat == F) { #run this if X is continuous
  a = round(coef(model2)[x],2)
  b = round(coef(model3)[m],2)
  c = round(coef(model1)[x],2)
  cprime = round(coef(model3)[x],2)
  } else {
    a = NA; c = NA; cprime = NA
    b = round(coef(model3)[m],2)

    for (i in 1:length(levelsx)) {

      a[i] = round(coef(model2)[levelsx[i]],2)
      c[i] = round(coef(model1)[levelsx[i]],2)
      cprime[i] = round(coef(model3)[levelsx[i]],2)

      a[i] = paste(levelsx[i], " = ", a[i], sep = "")
      c[i] = paste(levelsx[i], " = ", c[i], sep = "")
      cprime[i] = paste(levelsx[i], " = ", cprime[i], sep = "")

    } #close for loop
  } #close categorical x loop

  a = paste(a, collapse = " ")
  a = paste("`", a, "`", sep = "")
  c = paste(c, collapse = " ")
  c = paste("`", c, "`", sep = "")
  cprime = paste(cprime, collapse = " ")
  cprime = paste("`", cprime, "`", sep = "")

  bvalues = matrix(nrow = 3, ncol = 3, byrow = TRUE,
                   data = c(0, a, 0,
                            0, 0, 0,
                            b, paste(c, " (", cprime, ")", sep = ""), 0))
  plotmat(bvalues,
         pos = c(1,2),
         name = c(m, x, y),
         box.type = "rect",
         box.size = 0.12,
         box.prop = 0.5,
         curve = 0,
         shadow.size = 0)
  triangle = recordPlot()

  return(triangle)
}

#' @rdname draw.med
#' @export
