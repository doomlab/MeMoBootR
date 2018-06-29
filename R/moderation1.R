#' Two Way Moderation
#'
#' This function runs a complete two way moderation analysis with one
#' moderator, similiar to model 1 in PROCESS by A. Hayes (2013).
#' As part of the output, you will find data screening,
#' the overall model, and the simple slopes for X at each level of the moderator.
#' X and M variables will be mean centered after data screening and
#' before analysis to control for multicollinearity unless they are categorical.
#'
#' @param y The dependent variable column name from your dataframe.
#' @param x The independent variable column name from your dataframe. This column
#' will be treated as X in mediation or moderation models, please see
#' diagrams online for examples.
#' @param m The moderator for your model, as this model only includes one moderator.
#' @param cvs The covariates you would like to include in the model.
#' Use a \code{c()} concatenated vector to use multiple covariates.
#' @param df The dataframe where the columns from the formula can be found.
#' Note that only the columns used in the analysis will be data screened.
#' @param with_out A logical value where you want to keep the outliers in
#' model \code{TRUE} or exclude them from the model \code{FALSE}.
#' @keywords mediation, moderation, regression, data screening, bootstrapping
#' @export
#' @examples
#' states = as.data.frame(state.x77)
#' moderation1(y = "Income", x = "Illiteracy", m = "Murder",
#'           cvs = c("Population", "Area"), df = states)
#' @export

moderation1 = function(y, x, m, cvs = NULL, df, with_out = T) {

  #graph information
  require(ggplot2)
  cleanup = theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line.x = element_line(color = "black"),
                  axis.line.y = element_line(color = "black"),
                  legend.key = element_rect(fill = "white"),
                  text = element_text(size = 15))

  #stop if Y is categorical
  if (is.factor(df[ , y])){stop("Y should not be a categorical variable. Log regression options are coming soon.")}

  #first create the full formula for data screening
  allformulas = createformula(y, x, m, cvs, type = "moderation1")

  #then do data screening
  screen = datascreen(allformulas$eq1, df, with_out)

  #take out outlines and create finaldata
  if (with_out == F) { finaldata = subset(screen$fulldata, totalout < 2) } else { finaldata = screen$fulldata }

  #center x and m in the finaldata
  if (!is.factor(finaldata[ , x])){finaldata[ , x] = scale(finaldata[ , x], scale = F)}
  if (!is.factor(finaldata[ , m])){finaldata[ , m] = scale(finaldata[ , m], scale = F)}

  model1 = lm(allformulas$eq1, data = finaldata) #full model

  #create simple slopes
  finaldata$lowM = finaldata[ , m] + sd(finaldata[ , m])
  finaldata$highM = finaldata[ , m] - sd(finaldata[ , m])

  model1low = lm(allformulas$eq2, data = finaldata) #low simple slope
  model1high = lm(allformulas$eq3, data = finaldata) #high simple slope

  simslopes = paste("At low levels of ", m, ", you see that every unit increase in ",
                    x, " predicts ", round(coef(model1low)[x],2), " unit change in ", y,
                    ". \n\nAt average levels of ", m, ", you see that every unit increase in ",
                    x, " predicts ", round(coef(model1)[x],2), " unit change in ", y,
                    ". \n\nAt high levels of ", m, ", you see that every unit increase in ",
                    x, " predicts ", round(coef(model1high)[x],2), " unit change in ", y,
                    ".", sep = "")

  lowlabel = paste("-1SD ", m, sep = "")
  avglabel = paste("Average ", m, sep = "")
  highlabel = paste("+1SD ", m, sep = "")

  plot_sim = ggplot(finaldata, aes(finaldata[ , x],finaldata[ , y])) +
    xlab(x) +
    ylab(y) +
    geom_point() +
    scale_size_continuous(guide = FALSE) +
    geom_abline(aes(intercept = coef(model1low)["(Intercept)"], slope = coef(model1low)[x], linetype = lowlabel)) +
    geom_abline(aes(intercept = coef(model1)["(Intercept)"], slope = coef(model1)[x], linetype = avglabel)) +
    geom_abline(aes(intercept = coef(model1high)["(Intercept)"], slope = coef(model1high)[x], linetype = highlabel)) +
    scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                          breaks = c(lowlabel, avglabel, highlabel),
                          name = "Simple Slope") +
    coord_cartesian(xlim = c(min(finaldata[, x]), max(finaldata[, x])),
                    ylim = c(min(finaldata[, y]), max(finaldata[, y]))) +
    cleanup +
    NULL

  return(list("datascreening" = screen,
              "model1" = model1,
              "model1low" = model1low,
              "model1high" = model1high,
              "interpretation" = simslopes,
              "graphslopes" = plot_sim
  ))
}

#' @rdname mediation1
#' @export
