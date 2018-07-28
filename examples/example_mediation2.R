##An example of mediation 2
#download the data from our github examples page
master = read.csv("mediation2.csv")

##install if you need to
devtools::install_github("doomlab/MeMoBootR")

##load the library
library(MeMoBootR)

saved = mediation2(y = "Q11", #DV
                   x = "Q151", #IV
                   m1 = "Q31", #Mediator 1
                   m2 = "Q41", #Mediator 2
                   cvs = c("Q121"), #Any covariates
                   df = master, #Dataframe
                   with_out = F, #Not required but can change to F for no outliers
                   nboot = 1000, #Number of bootstraps
                   conf_level = .95 #CI width
                   )

####view data screening####
#outlier information is in the DF
View(saved$datascreening$fulldata)
sum(saved$datascreening$fulldata$totalout >=2)

#additivity
saved$datascreening$correl

#linearity
saved$datascreening$linearity

#normality
saved$datascreening$normality

#homogs
saved$datascreening$homogen

####view the analysis####
summary(saved$model1) #c path

#overall grade predicts overall course b = 0.41, t(3588) = 17.69, p < .001
#wanted to take predicts overall course b = 0.37, t(3588) = 31.50, p < .001

summary(saved$model2) #a1 path

#a1 path is x predicting m1
#overall grade predicts exam fairness b = 0.45, t(3588) = 23.26, p < .001

summary(saved$model3) #a2 d21 paths

#a2 path is x predicting m2
#overall grade predicts grading fairness b = 0.11, t(3587) = 8.77, p < .001
#d21 path is m1 predicting m2
#exam fairness predicts grading fairness b = 0.56, t(3587) = 14.34, p < .001

summary(saved$model4) #b1, b2, c' paths

#b1 m1 predicting y
#exam fairness predicts overall course rating b = 0.58, t(3586) = 30.12, p < .001
#b2 m2 predicting y
#overall grading fairness predicts overall course rating
#b = 0.41, t(3586) = 18.01, p < .001
#c' path x predicting y
#overall grade does not predict overall course b < 0.01, t(3586) = 0.07, p = .944

#total, direct, indirect effects
saved$total.effect
saved$direct.effect
saved$indirect.effect1 #x through m1
saved$indirect.effect2 #x through m2
saved$indirect.effect3 #x through m1 through m2

#bootstrapped indirect
saved$boot.results

#bootstrapped CI
saved$boot.ci
#or
saved$boot.ci$Q151.1

#indirect effect of exam fairness between overall grading and overall course
#indirect = 0.26, SE = 0.02, 95% CI[0.22, 0.29]

saved$boot.ci$Q151.2 #repeat above process to write out
saved$boot.ci$Q151.3

##how do i know if mediation occurs ? if the CI does NOT include zero
#all three indirects show mediation effects

####power####
library(pwr)
##power runs on cohen's f - not to be confused with anova f.
##take the R squared to convert
R2 =  .60
feta = R2 / (1-R2)

#u is df model, which is number of predictors
#v is df error, but we are trying to figure out
#sample size so we leave this one blank.
#f2 is cohen f squared

pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size
