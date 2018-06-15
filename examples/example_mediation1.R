##An example of mediation 1

##install if you need to
devtools::install_github("doomlab/MeMoBootR")

##load the library
library(MeMoBootR)

saved = mediation1(y = "mpg", #DV
                   x = "hp", #IV
                   m = "cyl",  #Mediator
                   #cvs = c("wt", "gear"), #Any covariates
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

#total, direct, indirect effects
saved$total.effect; saved$direct.effect; saved$indirect.effect

#Sobel test
saved$z.score; saved$p.value

#bootstrapped indirect
saved$boot.results

#bootstrapped CI
saved$boot.ci
