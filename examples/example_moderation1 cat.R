#An example of two way moderation with covariates

##install if you need to
devtools::install_github("doomlab/MeMoBootR")

##load the library
library(MeMoBootR)

#load the dataset from R
states = as.data.frame(state.x77)

#create fake categorical variable
states$Murder_cat = states$Murder
states$Murder_cat = as.factor(ifelse(states$Murder < 5, "Low",
                                     ifelse(states$Murder < 9, "Average", "High")))
states$Murder_cat = factor(states$Murder_cat,
                           levels = c("Low", "Average", "High"))

saved = moderation1(y = "Income", #DV
                    x = "Illiteracy", #IV
                    m = "Murder_cat", #Moderator for simple slopes
                    cvs = NULL, #covariates
                    df = states, #data frame of columns
                    with_out = T)

#View the outliers
View(saved$datascreening$fulldata)

#Additivity/multicollinearity
saved$datascreening$correl

#Linearity
saved$datascreening$linearity

#Normality
saved$datascreening$normality

#Homogeneity + Homoscedasticity
saved$datascreening$homogen

#Overall Model (Average Simple Slopes)
summary(saved$model1)

##overall model statistics

##each predictor one at a time

##Interaction

#Simple slopes by levels of M
saved$slopemodels

##view with lapply
lapply(saved$slopemodels, summary)

##or pull one at a time


#Interpretation of Slopes
cat(saved$interpretation)

#Graph of the Slopes
saved$graphslopes

#remember, you can run the function with out ()
#to view the graph code if you want to tweak it more
moderation1

####Power####
library(pwr)
##power runs on cohen's f - not to be confused with anova f.
##take the R squared to convert
R2 =  .11
feta = R2 / (1-R2)

#u is df model, which is number of predictors
#v is df error, but we are trying to figure out
#sample size so we leave this one blank.
#f2 is cohen f squared

##all the predictors
pwr.f2.test(u = 5, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size

##addition of the interaction only
pwr.f2.test(u = 1, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size
