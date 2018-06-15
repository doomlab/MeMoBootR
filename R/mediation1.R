#' mediation1
#'
#' This function runs a complete simple mediation analysis with one mediator, similiar to model 1 in PROCESS by A. Hayes (2013). 
#' As part of the output, you will find data screening, 
#' all three models used in the traditional Baron and 
#' Kenny (1986) steps, total/direct/indirect effects, the z-score and p-value
#' for the Aroian Sobel test, and the bootstrapped confidence interval for the indirect effect.
#' 
#' @param y The dependent variable column name from your dataframe.
#' @param x The independent variable column name from your dataframe. This column will be treated as X in mediation or moderation models, please see diagrams online for examples. 
#' @param m The mediator for your model, as this model only includes one mediator. 
#' @param cvs The covariates you would like to include in the model. Use a `c()` concatenated vector to use multiple covariates.
#' @param df The dataframe where the columns from the formula can be found. Note that only the columns used in the analysis will be data screened.
#' @param with_out A logical value where you want to keep the outliers in model `TRUE` or exclude them from the model `FALSE`.
#' @param nboot A numeric value indicating the number of bootstraps you would like to complete.
#' @param conf_level A numeric value indicating the confidence interval width for the boostrapped confidence interval.
#' @keywords mediation, moderation, regression, data screening, bootstrapping
#' @export 
#' @examples 
#' mediation1(y = "cyl", x = "mpg", m = "disp", cvs = c("drat", "gear"))


mediation1 = function(y, x, m, cvs = NULL, df, with_out = T, nboot = 1000, conf_level = .95) {
  
  require(boot)
  
  #first create the full formula for data screening
  allformulas = createformula(y, x, m, cvs)
  
  #then do data screening
  screen = datascreen(allformulas$eq3, df, with_out)
  
  #take out outlines and create finaldata
  if (with_out == F) { finaldata = subset(screen$fulldata, totalout < 2) } else { finaldata = screen$fulldata }
  
  model1 = lm(allformulas$eq1, data = finaldata) #c path
  model2 = lm(allformulas$eq2, data = finaldata) #a path
  model3 = lm(allformulas$eq3, data = finaldata) #b c' paths
  
  #aroian sobel
  a = coef(model2)[2]
  b = coef(model3)[3]
  SEa = summary(model2)$coefficients[2,2]
  SEb = summary(model3)$coefficients[3,2]
  zscore = (a*b)/(sqrt((b^2*SEa^2)+(a^2*SEb^2)+(SEa*SEb)))
  pvalue = pnorm(abs(zscore), lower.tail = F)*2
  
  #reporting
  total = coef(model1)[2] #c path
  direct = coef(model3)[2] #c' path
  indirect = a*b
  
  bootresults = boot(data = finaldata,
                     statistic = indirectm1,
                     formula2 = allformulas$eq2,
                     formula3 = allformulas$eq3,
                     R = nboot)
  
  bootci = boot.ci(bootresults,
                   conf = conf_level,
                   type = "norm")
  
  return(list("datascreening" = screen,
              "model1" = model1,
              "model2" = model2,
              "model3" = model3,
              "total.effect" = total,
              "direct.effect" = direct,
              "indirect.effect" = indirect,
              "z.score" = zscore,
              "p.value" = pvalue,
              "boot.results" = bootresults,
              "boot.ci" = bootci
  ))
}