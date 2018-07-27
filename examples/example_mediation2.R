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
                   with_out = T, #Not required but can change to F for no outliers
                   nboot = 1000, #Number of bootstraps
                   conf_level = .95 #CI width
                   )

####view data screening####
#outlier information is in the DF
View(saved$datascreening$fulldata)

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
summary(saved$model2) #a1 path
summary(saved$model3) #a2 d21 paths
summary(saved$model4) #b1, b2, c' paths

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
saved$boot.ci$Q151.2
saved$boot.ci$Q151.3

####power####
library(pwr)
##power runs on cohen's f - not to be confused with anova f.
##take the R squared to convert
R2 =  .12
feta = R2 / (1-R2)

#u is df model, which is number of predictors
#v is df error, but we are trying to figure out
#sample size so we leave this one blank.
#f2 is cohen f squared

pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size
