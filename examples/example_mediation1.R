##An example of mediation 1

##install if you need to
devtools::install_github("doomlab/MeMoBootR")

##load the library
library(MeMoBootR)

saved = mediation1(y = "mpg", #DV
                   x = "cyl", #IV
                   m = "hp",  #Mediator
                   cvs = c("wt", "gear"), #Any covariates
                   df = mtcars, #Dataframe
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
summary(saved$model2) #a path
summary(saved$model3) #b and c' path

#X predicts Y total effects
#c path b = -1.52, t(28) = -3.64, p = .001
#F(3,28) = 46.53, p < .001, R2 = .83

#X predicts M
#a path b = 32.97, t(28) = 6.64, p < .001

#X predicts Y with M direct effects
#c' path b = -0.81, t(27) = -1.23, p = .231
#M predicts Y with X 
#b path b = -0.02, t(27) = -1.38, p = .179

#total, direct, indirect effects
saved$total.effect; saved$direct.effect; saved$indirect.effect

#Sobel test
saved$z.score; saved$p.value

#bootstrapped indirect
saved$boot.results

#bootstrapped CI
saved$boot.ci

#indirect = -0.72, SE = 0.52, 95% CI[-1.81, 0.22]

####power####
library(pwr)
##power runs on cohen's f - not to be confused with anova f. 
##take the R squared to convert
R2 =  .06
feta = R2 / (1-R2)

#u is df model, which is number of predictors 
#v is df error, but we are trying to figure out 
#sample size so we leave this one blank. 
#f2 is cohen f squared 

pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size 