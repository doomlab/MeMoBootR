#An example of two way moderation with covariates

##install if you need to
devtools::install_github("doomlab/MeMoBootR")

##load the library
library(MeMoBootR)

#load the dataset from R
states = as.data.frame(state.x77)

saved = moderation1(y = "Income", #DV
                    x = "Illiteracy", #IV
                    m = "Murder", #Moderator for simple slopes
                    cvs = c("Population", "Area"), #covariates
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
#F(5, 44) = 10.30, p < .001, R2 = .54

##each predictor one at a time
#Illiteracy b = -114.78, t(44) = -0.67, p = .508, not significant predictor
#Murder, also not signifificant
#Population, as population increases, income increases
#Area, as area increases, income increases
#Interaction b = -115.56, t(44) = -3.36, p = .002 - significant
#difficult to interpret...

#Low Simple Slope Model
summary(saved$model1low)

#look for is X because M is the slope we are manipulating
#Illiteracy not predictor of Income

#High Simple Slope Model
summary(saved$model1high)

#Illiteracy is predictor of income

#Interpretation of Slopes
cat(saved$interpretation)

##what does it mean if the interaction is significant but none of the simple
##slopes are significant...??
###implies the simple slopes are changing/different but that the main effect
###X to Y is not significant

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
