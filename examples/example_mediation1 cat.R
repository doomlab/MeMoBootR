##An example of mediation 1 with categorical variables

##install if you need to
devtools::install_github("doomlab/MeMoBootR")

##load the library
library(MeMoBootR)

data(mtcars)
mtcars$cyl = as.factor(mtcars$cyl)

saved = mediation1(y = "mpg", #DV
                   x = "cyl", #IV
                   m = "wt",  #Mediator
                   cvs = NULL, #Any covariates
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

#X predicts M

#X predicts Y with M direct effects

#M predicts Y with X

#total, direct, indirect effects
saved$total.effect; saved$direct.effect; saved$indirect.effect

#Sobel test
saved$z.score; saved$p.value

#bootstrapped indirect
saved$boot.results

#bootstrapped CI
saved$boot.ci

####power####
library(pwr)
##power runs on cohen's f - not to be confused with anova f.
##take the R squared to convert
R2 =  .14
feta = R2 / (1-R2)

#u is df model, which is number of predictors
#v is df error, but we are trying to figure out
#sample size so we leave this one blank.
#f2 is cohen f squared

pwr.f2.test(u = 3, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size
