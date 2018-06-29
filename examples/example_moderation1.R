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
                    df = states) #data frame of columns

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

#Low Simple Slope Model
summary(saved$model1low)

#High Simple Slope Model
summary(saved$model1high)

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
R2 =  .02
feta = R2 / (1-R2)

#u is df model, which is number of predictors 
#v is df error, but we are trying to figure out 
#sample size so we leave this one blank. 
#f2 is cohen f squared 

pwr.f2.test(u = 5, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size 
