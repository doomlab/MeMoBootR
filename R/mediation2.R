#' Serial Mediation with Two Mediators
#'
#' This function runs a complete serial mediation analysis with two
#' mediator, similiar to model 6 in PROCESS by A. Hayes (2013).
#' As part of the output, you will find data screening,
#' all three models used in the traditional Baron and
#' Kenny (1986) steps, total/direct/indirect effects, the z-score and p-value
#' for the Aroian Sobel test, and the bootstrapped confidence interval
#' for the indirect effect.
#'
#' @param y The dependent variable column name from your dataframe.
#' @param x The independent variable column name from your dataframe. This column
#' will be treated as X in mediation or moderation models, please see
#' diagrams online for examples.
#' @param m1 The first mediator for your model.
#' @param m2 The second mediator for your model.
#' @param cvs The covariates you would like to include in the model.
#' Use a \code{c()} concatenated vector to use multiple covariates.
#' @param df The dataframe where the columns from the formula can be found.
#' Note that only the columns used in the analysis will be data screened.
#' @param with_out A logical value where you want to keep the outliers in
#' model \code{TRUE} or exclude them from the model \code{FALSE}.
#' @param nboot A numeric value indicating the number of bootstraps you would like to complete.
#' @param conf_level A numeric value indicating the confidence interval width for the boostrapped confidence interval.
#' @keywords mediation, regression, data screening, bootstrapping
#' @export
#' @examples
#' mediation2(y = "Q11", x = "Q151", m1 = "Q31", m2 = "Q41",
#'           cvs = c("Q121"), df = mediation2, nboot = 1000, with_out = T,
#'           conf_level = .95)
#' @export

mediation2 = function(y, x, m1, m2, cvs = NULL, df, with_out = T,
                      nboot = 1000, conf_level = .95) {

  require(boot)

  #stop if Y is categorical
  if (is.factor(df[ , y])){stop("Y should not be a categorical variable. Log regression options are coming soon.")}

  #stop if M is categorical
  if (is.factor(df[ , m1])){stop("M1 should not be a categorial variable.")}
  if (is.factor(df[ , m2])){stop("M2 should not be a categorial variable.")}

  #figure out if X is categorical
  if (is.factor(df[ , x])){xcat = TRUE} else {xcat = FALSE}

  #first create the full formula for data screening
  allformulas = createformula(y, x, m = m1, m2, cvs, type = "mediation2")

  #then do data screening
  screen = datascreen(allformulas$eq4, df, with_out)

  #take out outlines and create finaldata
  if (with_out == F) { finaldata = subset(screen$fulldata, totalout < 2) } else { finaldata = screen$fulldata }

  model1 = lm(allformulas$eq1, data = finaldata) #c path
  model2 = lm(allformulas$eq2, data = finaldata) #a1 path
  model3 = lm(allformulas$eq3, data = finaldata) #a2 d21 path
  model4 = lm(allformulas$eq4, data = finaldata) #b2 c' paths

  if (xcat == F){ #run this with continuous X

  #relevant coefficients
  a1 = coef(model2)[x]
  b1 = coef(model4)[m1]
  a2 = coef(model3)[x]
  b2 = coef(model4)[m2]
  d21 = coef(model3)[m1]

  #reporting
  total = coef(model1)[x] #c path
  direct = coef(model4)[x] #c' path
  indirect1 = a1*b1
  indirect2 = a2*b2
  indirect3 = a1*d21*b2

  } else {

    #figure out all the labels for X
    levelsx = paste(x, levels(df[, x])[-1], sep = "")
    total = NA; indirect1 = NA; indirect2 = NA; indirect3 = NA
    direct = NA; zscore = NA; pvalue = NA

    #loop over that to figure out sobel and reporting
    for (i in 1:length(levelsx)){

      #relevant coefficients
      a1 = coef(model2)[levelsx[i]]
      b1 = coef(model4)[m1]
      a2 = coef(model3)[levelsx[i]]
      b2 = coef(model4)[m2]
      d21 = coef(model3)[m1]

      #reporting
      total[i] = coef(model1)[levelsx[i]] #c path
      direct[i] = coef(model4)[levelsx[i]] #c' path
      indirect1[i] = a1*b1
      indirect2[i] = a2*b2
      indirect3[i] = a1*d21*b2

    } #close for loop
  } #close else x is categorical

  bootresults = boot(data = finaldata,
                     statistic = indirectmed2,
                     formula2 = allformulas$eq2,
                     formula3 = allformulas$eq3,
                     formula4 = allformulas$eq4,
                     x = x,
                     m1 = m1,
                     m2 = m2,
                     R = nboot)

  if (xcat == F) { #run this if X is continuous
    bootci = list()

    for (i in 1:length(bootresults$t0)) {

      bootci[[i]] = boot.ci(bootresults,
                   conf = conf_level,
                   type = "norm",
                   index = i)
      names(bootci)[[i]] = paste(names(bootresults$t0)[[i]], ".", i, sep = "")
      }

  } else {
    bootci = list()
    sim = 1
    for (i in 1:length(levelsx)){ #loop over categorical x
      for (r in 1:length(bootresults$t0)) { #loop over multiple bootstraps

        bootci[[sim]] = boot.ci(bootresults,
                          conf = conf_level,
                          type = "norm",
                          index = sim)

        names(bootci)[[sim]] = paste(levelsx[[i]], ".", names(bootresults$t0)[[r]], sep = "")
        sim = sim + 1
      } #close boot index

    } #close levels index

  } #close else statement

  return(list("datascreening" = screen,
              "model1" = model1,
              "model2" = model2,
              "model3" = model3,
              "model4" = model4,
              "total.effect" = total,
              "direct.effect" = direct,
              "indirect.effect1" = indirect1,
              "indirect.effect2" = indirect2,
              "indirect.effect3" = indirect3,
              "boot.results" = bootresults,
              "boot.ci" = bootci
  ))
}

#' @rdname mediation2
#' @export
