#' Double Two-Way Moderation
#'
#' This function runs a complete double two-way moderation analysis with two
#' moderators, similiar to model 2 in PROCESS by A. Hayes (2013).
#' As part of the output, you will find data screening,
#' the overall model, and the simple slopes for X at each level of the moderator.
#' X and M variables will be mean centered after data screening and
#' before analysis to control for multicollinearity unless they are categorical.
#'
#' @param y The dependent variable column name from your dataframe.
#' @param x The independent variable column name from your dataframe. This column
#' will be treated as X in mediation or moderation models, please see
#' diagrams online for examples.
#' @param m1 The first moderator for your model.
#' @param m2 The second moderator for your model.
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
#' moderation2(y = "Income", x = "Illiteracy", m1 = "Murder",
#'           m2 = "Population", df = states)
#' @export

moderation2 = function(y, x, m1, m2, cvs = NULL, df, with_out = T) {

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

  #stop if X is categorical
  if (is.factor(df[ , x])){stop("X should not be categorical, please put categorical predictors as M (or use ANOVA for double categorical variables).")}

  #first create the full formula for data screening
  allformulas = createformula(y = y, x = x, m = m1,
                              m2 = m2, cvs = cvs, type = "moderation2")

  #then do data screening
  screen = datascreen(allformulas$eq1, df, with_out)

  #take out outlines and create finaldata
  if (with_out == F) { finaldata = subset(screen$fulldata, totalout < 2) } else { finaldata = screen$fulldata }

  #center x and m in the finaldata, create simple slopes for continuous variables
  if (!is.factor(finaldata[ , x])){finaldata[ , x] = scale(finaldata[ , x], scale = F)}
  if (!is.factor(finaldata[ , m1])){
    finaldata[ , m1] = scale(finaldata[ , m1], scale = F)
    finaldata$lowM1 = finaldata[ , m1] + sd(finaldata[ , m1])
    finaldata$highM1 = finaldata[ , m1] - sd(finaldata[ , m1])
    }
  if (!is.factor(finaldata[ , m2])){
    finaldata[ , m2] = scale(finaldata[ , m2], scale = F)
    finaldata$lowM2 = finaldata[ , m2] + sd(finaldata[ , m2])
    finaldata$highM2 = finaldata[ , m2] - sd(finaldata[ , m2])
    }

  model1 = lm(allformulas$eq1, data = finaldata) #full model

  #run simple slopes for all continuous categorical combinations

  ####both continuous####
  if (!is.factor(finaldata[ , m1]) & !is.factor(finaldata[ , m2])){

    model1.1 = lm(allformulas$eq1.1, data = finaldata)
    model1.2 = lm(allformulas$eq1.2, data = finaldata)
    model2 = lm(allformulas$eq2, data = finaldata)
    model2.1 = lm(allformulas$eq2.1, data = finaldata)
    model2.2 = lm(allformulas$eq2.2, data = finaldata)
    model3 = lm(allformulas$eq3, data = finaldata)
    model3.1 = lm(allformulas$eq3.1, data = finaldata)
    model3.2 = lm(allformulas$eq3.2, data = finaldata)

    simslopes = data.frame(row.names = c(paste("Low",m1),
                                         paste("Average",m1),
                                         paste("High",m1)),
                           "Low" = c(coef(model2.1)[x], coef(model1.1)[x], coef(model3.1)[x]),
                           "Average" = c(coef(model2)[x], coef(model1)[x], coef(model3)[x]),
                           "High" = c(coef(model2.2)[x], coef(model1.2)[x], coef(model3.2)[x])
    )
    colnames(simslopes) = c(paste("Low", m2), paste("Average", m2), paste("High", m2))

    #graphs
    lowlabel = paste("-1SD ", m2, sep = "")
    avglabel = paste("Average ", m2, sep = "")
    highlabel = paste("+1SD ", m2, sep = "")

    #low m1
    plot_sim_low = ggplot(finaldata, aes(finaldata[ , x],finaldata[ , y])) +
      xlab(x) +
      ylab(y) +
      geom_point(alpha = .3) +
      scale_size_continuous(guide = FALSE) +
      geom_abline(aes(intercept = coef(model2.1)["(Intercept)"], slope = coef(model2.1)[x], linetype = lowlabel)) +
      geom_abline(aes(intercept = coef(model2)["(Intercept)"], slope = coef(model2)[x], linetype = avglabel)) +
      geom_abline(aes(intercept = coef(model2.2)["(Intercept)"], slope = coef(model2.2)[x], linetype = highlabel)) +
      scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                            breaks = c(lowlabel, avglabel, highlabel),
                            name = "Simple Slope") +
      coord_cartesian(xlim = c(min(finaldata[, x]), max(finaldata[, x])),
                      ylim = c(min(finaldata[, y]), max(finaldata[, y]))) +
      cleanup +
      NULL

    #average m1
    plot_sim_average = ggplot(finaldata, aes(finaldata[ , x],finaldata[ , y])) +
      xlab(x) +
      ylab(y) +
      geom_point(alpha = .3) +
      scale_size_continuous(guide = FALSE) +
      geom_abline(aes(intercept = coef(model1.1)["(Intercept)"], slope = coef(model1.1)[x], linetype = lowlabel)) +
      geom_abline(aes(intercept = coef(model1)["(Intercept)"], slope = coef(model1)[x], linetype = avglabel)) +
      geom_abline(aes(intercept = coef(model1.2)["(Intercept)"], slope = coef(model1.2)[x], linetype = highlabel)) +
      scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                            breaks = c(lowlabel, avglabel, highlabel),
                            name = "Simple Slope") +
      coord_cartesian(xlim = c(min(finaldata[, x]), max(finaldata[, x])),
                      ylim = c(min(finaldata[, y]), max(finaldata[, y]))) +
      cleanup +
      NULL

    #high m1
    plot_sim_high = ggplot(finaldata, aes(finaldata[ , x],finaldata[ , y])) +
      xlab(x) +
      ylab(y) +
      geom_point(alpha = .3) +
      scale_size_continuous(guide = FALSE) +
      geom_abline(aes(intercept = coef(model3.1)["(Intercept)"], slope = coef(model3.1)[x], linetype = lowlabel)) +
      geom_abline(aes(intercept = coef(model3)["(Intercept)"], slope = coef(model3)[x], linetype = avglabel)) +
      geom_abline(aes(intercept = coef(model3.2)["(Intercept)"], slope = coef(model3.2)[x], linetype = highlabel)) +
      scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                            breaks = c(lowlabel, avglabel, highlabel),
                            name = "Simple Slope") +
      coord_cartesian(xlim = c(min(finaldata[, x]), max(finaldata[, x])),
                      ylim = c(min(finaldata[, y]), max(finaldata[, y]))) +
      cleanup +
      NULL

    return(list("datascreening" = screen,
                "avgm1_avgm2" = model1,
                "avgm1_lowm2" = model1.1,
                "avgm1_highm2" = model1.2,
                "lowm1_avgm2" = model2,
                "lowm1_lowm2" = model2.1,
                "lowm1_highm2" = model2.2,
                "highm1_avgm2" = model3,
                "highm1_lowm2" = model3.1,
                "highm1_highm2" = model3.2,
                "interpretation" = simslopes,
                "lowm1_graph" = plot_sim_low,
                "avgm1_graph" = plot_sim_average,
                "highm1_graph" = plot_sim_high))
  }

  ####m1 categorical, m2 continuous####
  if (is.factor(finaldata[ , m1]) & !is.factor(finaldata[ , m2])){
    return("Coming soon!")
  }

  ####m1 continous, m2 categorical####
  if (!is.factor(finaldata[ , m1]) & is.factor(finaldata[ , m2])){
    return("Coming soon!")
  }

  ####m1 categorical, m2 catorgical####
  if (is.factor(finaldata[ , m1]) & is.factor(finaldata[ , m2])){
    return("Coming soon!")
  }

  }

#' @rdname moderation2
#' @export
