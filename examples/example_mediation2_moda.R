# Serial Mediation with Moderation on A1 Path

# Install package
library(devtools)
install_github("doomlab/MeMoBootR")

# Toy data
set.seed(123)
N <- 200
X <- rep(c("C", "T"), each = 100) # c = control vs. t = treatment
M1 <- c(rnorm(100, 1.17, .2), rnorm(100, 2.99, .2))
M2 <- 1.2*M1 + rnorm(N, 2.5, .2)
Y <- 0.2*M2 + rnorm(N, 2.5, .2)
W <- 0.7*M1 + rnorm(N, 2.5, .2)
data1 <- data.frame(X, M1, M2, W, Y)

# Demo of new MeMoBootR function
library(MeMoBootR)

saved <- mediation2_moda(y = "Y",
                         x = "X",
                         m1 = "M1",
                         m2 = "M2",
                         mod = "W",
                         cvs = NULL,
                         df = data1,
                         with_out = T,
                         nboot = 1000,
                         conf_level = .95)

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
## note that all X, M1, M2, W are centered
## unless they are categorical

summary(saved$model1) #c path

summary(saved$model2) #a1_avg path
summary(saved$model2_low) #a1_low path
summary(saved$model2_high) #a2_high path

summary(saved$model3) #a2 d21 paths

summary(saved$model4) #b1, b2, c' paths

#total, direct, indirect effects
saved$total.effect
saved$direct.effect
saved$indirect.effect1_avg #x through m1 when w is avg
saved$indirect.effect1_low #x through m2 when w is low
saved$indirect.effect1_high #x through m2 when w is high
saved$indirect.effect2 #x through m2
saved$indirect.effect3_avg #x through m1 through m2 when w is avg
saved$indirect.effect3_low #x through m1 through m2 when w is low
saved$indirect.effect3_high #x through m1 through m2 when w is high

#bootstrapped indirect
saved$boot.results

#bootstrapped CI
# need to give these better names
saved$boot.ci

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


