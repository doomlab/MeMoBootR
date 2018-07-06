#An example of two way moderation with categorical M

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
#F(5,44) = 5.23, p = .001, R2 = .37

##each predictor one at a time
#Illiteracy b = 622.70, t(44) = 1.09, p = .280
#illiteracy does not predict income rates

#Murder Average versus low b = -291.80, t(44) = -0.92, p = .363
#not a significant difference between low and average murder rates for income
#Murder High versus low b = -122.80, ... not significant
tapply(states$Income, states$Murder_cat, mean)

##Interaction
#Illiteracy by average versus low not significant
#Illiteracy by high versus low is significant

#break down by M
#illiteracy predicting income for each group/level of murder

#Simple slopes by levels of M
saved$slopemodels

##view with lapply
lapply(saved$slopemodels, summary)

#Low murder levels, illiteracy b = 622.70 not significant
#Average b = 125.00 not significant
#High b = -902.60 is significant

#at low and average murder rates, illiteracy does not predict income
#at high murder rates, illiteracy negatively predicts income

##or pull one at a time
summary(saved$slopemodels$Low)
coef(saved$slopemodels$Low)

summary(saved$slopemodels$Average)

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
R2 =  .20
feta = R2 / (1-R2)

#u is df model, which is number of predictors
#v is df error, but we are trying to figure out
#sample size so we leave this one blank.
#f2 is cohen f squared

##all the predictors
pwr.f2.test(u = 5, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size

##addition of the interaction only
pwr.f2.test(u = 2, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size
