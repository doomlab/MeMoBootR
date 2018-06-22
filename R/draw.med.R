#' Simple Mediation Diagrams
#'
#' This function returns the code to create a simple mediation triangle.
#'
#' @param y The dependent variable column name from your dataframe.
#' @param x The independent variable column name from your dataframe. This column
#' will be treated as X in mediation or moderation models, please see
#' diagrams online for examples.
#' @param m The mediator for your model, as this model only includes one mediator.
#' @param cvs The covariates you would like to include in the model.
#' Use a \code{c()} concatenated vector to use multiple covariates.
#' @param df The dataframe where the columns from the formula can be found.
#' Note that only the columns used in the analysis will be data screened.
#' @param with_out A logical value where you want to keep the outliers in
#' model \code{TRUE} or exclude them from the model \code{FALSE}.
#' @param nboot A numeric value indicating the number of bootstraps you would like to complete.
#' @param conf_level A numeric value indicating the confidence interval width for the boostrapped confidence interval.
#' @keywords mediation, moderation, regression, data screening, bootstrapping
#' @export
#' @examples
#' mediation1(y = "cyl", x = "mpg", m = "disp",
#'           cvs = c("drat", "gear"))
#' @export


library(diagram)
data <- c(0, "'.47*'", 0, 0,
          0, 0, 0, 0, 
          "'.36*'", "'.33* (.16)'", 0, 0,
          0, 0, 0, 0)
M<- matrix (nrow=4, ncol=4, byrow = TRUE, data=data)
plot<- plotmat(M, #matrix of coefficients rows = to, columns = from 
               pos=c(1,2,1), #number of elements in each row
               name= c( "Math self-efficacy","Math ability", "Interest in the math major", "stuff"),
               #vector of names
               box.type = "rect", 
               box.size = 0.12, 
               box.prop = 0.5,
               curve=0)
