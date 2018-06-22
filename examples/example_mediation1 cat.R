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
#4v6 b = -6.92, t(29) = -4.44, p < .001
#4v8 b = -11.56, t(29) = -8.91, p < .001
#interpretation difference between groups (X) on Y
tapply(mtcars$mpg, mtcars$cyl, mean)

#X predicts M
#4v6 b = 0.83, t(29) = 2.73, p = .011
#4v8 b = 1.71, t(29) = 6.75, p < .001
tapply(mtcars$wt, mtcars$cyl,mean)

#X predicts Y with M direct effects
#4v6 b = -4.26, t(28) = -3.07, p = .005
#4v8 b = -6.07, t(28) = -3.67, p < .001

#M predicts Y with X
#b = -3.21, t(28) = -4.25, p < .001

#total, direct, indirect effects
saved$total.effect; saved$direct.effect; saved$indirect.effect

#Sobel test
saved$z.score; saved$p.value

#Z = -2.12, p = .034
#Z = -3.45, p < .001

#bootstrapped indirect
saved$boot.results

#bootstrapped CI
saved$boot.ci

#indirect = -3.54, 95% CI[-6.66, -0.26] does cross zero,
#so we would consider this mediation
#indirect = -10.40, 95% CI[-17.20, -3.35]

#diagram
saved$diagram

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
