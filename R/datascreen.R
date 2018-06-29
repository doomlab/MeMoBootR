#' Data Screening for Regression
#'
#' This function using the data screening procedures outlined
#' by Tabachnick and Fidell (2012) to analyze the data for outliers
#' using Mahalanobis distance, Cook's, and Leverage values.
#' Further, the output includes correlations for additivity/multicollinearity,
#' and plots for linearity, normality, and homogeneity/homoscedasticity.
#' You can learn more about the data screening procedure implemented here at
#' the Statistics of DOOM YouTube channel.
#'
#' @param eq The full equation with all x, m, and cvs created from the
#' \code{createformula()} function.
#' @param df The dataframe where the columns from the formula can
#' be found. Note that only the columns used in the analysis will be screened.
#' @param with_out A logical value where you want to keep the outliers in
#' the data screening \code{TRUE} or exclude them from the data screening \code{FALSE}.
#' @keywords mediation, moderation, regression, data screening
#' @export
#' @examples
#' datascreen(eq = "mpg ~ cyl + disp + drat + gear",
#'            df = mtcars, with_out = TRUE)
#' @export

datascreen = function(eq, df, with_out = T) {

  output = lm(eq, data = df)

  columnstopull = variable.names(output)[-1]

  mahalcolumns = columnstopull[columnstopull %in% colnames(df)]

  if (length(mahalcolumns) > 1){ #only run mahalanobis if there are continuous variables
  ##Mahal
  mahal = mahalanobis(df[ ,mahalcolumns],
                      colMeans(df[ , mahalcolumns]),
                      cov(df[ , mahalcolumns]))
  cutmahal = qchisq(1-.001, ncol(df[ , mahalcolumns]))
  badmahal = as.numeric(mahal > cutmahal)
  }

  ##leverage
  k = length(output$coefficients)-1 ##number of IVs
  leverage = hatvalues(output)
  cutleverage = (2*k+2) / nrow(df)
  badleverage = as.numeric(leverage > cutleverage)

  ##cooks
  cooks = cooks.distance(output)
  cutcooks = 4 / (nrow(df) - k - 1)
  badcooks = as.numeric(cooks > cutcooks)

  if (length(mahalcolumns) > 1) {
  ##totaloutliers
  totalout = badmahal + badleverage + badcooks
  ##make new data frame to return
  df2 = cbind(df, badmahal, badleverage, badcooks, totalout)
  } else {
    ##total outliers
    totalout = badleverage + badcooks
    ##make a new data frame to return
    df2 = cbind(df, badleverage, badcooks, totalout)
    }

  #run with or without outliers
  if (with_out == F) { finaldata = subset(df2, totalout < 2) } else { finaldata = df2 }

  output2 = lm(eq, data = finaldata)

  ##additivity
  get_correl = summary(output2, correlation = T)

  ##assumptions
  standardized = rstudent(output2)
  fitted = scale(output2$fitted.values)

  ##linearity
  qqnorm(standardized); abline(0,1)
  line_plot = recordPlot()

  ##normality
  hist(standardized)
  hist_plot = recordPlot()

  ##homog and s
  plot(fitted, standardized)
  abline(0,0)
  abline(v = 0)
  homogs_plot = recordPlot()

  return(list(fulldata = df2,
              correl = get_correl$correlation,
              linearity = line_plot,
              normality = hist_plot,
              homogen = homogs_plot))
}

#' @rdname datascreen
#' @export
