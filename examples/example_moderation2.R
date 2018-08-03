#An example of double two way moderation with covariates
#download the data from our github examples page
master = read.csv("mediation2.csv")

##install if you need to
devtools::install_github("doomlab/MeMoBootR")

##load the library
library(MeMoBootR)

saved = moderation2(y = "Q11", #DV
                    x = "Q151", #IV
                    m1 = "Q31", #Moderator 1
                    m2 = "Q41", #Moderator 2
                    cvs = c("Q121"), #Any covariates
                    df = master, #Dataframe
                    with_out = F #include outliers or not
                    )

#View the outliers
View(saved$datascreening$fulldata)
sum(saved$datascreening$fulldata$totalout >= 2)

#Additivity/multicollinearity
saved$datascreening$correl

#Linearity
saved$datascreening$linearity

#Normality
saved$datascreening$normality

#Homogeneity + Homoscedasticity
saved$datascreening$homogen

#Overall Model - Average M1, Average M2
summary(saved$avgm1_avgm2)

#this model is the overall model that you would run
#to determine if you even wanted to do simple slopes
#main effect
#course grade doesn't predict overall eval
#b = .004, t(3570) = 0.25, p = .805
#exam fairness does predict overall eval
#grade fairness does predict overall eval

#covariate adjustor
#course wanted to take predicts overall eval

#interaction course grade and exam b = -0.10, t(3570) = -1.84, p = .066
#interaction course grade and grade fairness is significant

#overall model
#F(6,3570) = 1148, p < .001, R2 = .659 all six predictors are significant

#there are eight more models with the combinations
#of m1 and m2 at low, average, high
summary(saved$avgm1_lowm2)
summary(saved$avgm1_highm2)

#you can view each one for reporting or look at X for each
View(saved$interpretation)
#each number is x predicting y at the "area" of the data

#Graph of the Slopes by M1
saved$lowm1_graph
saved$avgm1_graph
saved$highm1_graph

####Power####
library(pwr)
##power runs on cohen's f - not to be confused with anova f.
##take the R squared to convert
R2 =  .03
feta = R2 / (1-R2)

#u is df model, which is number of predictors
#v is df error, but we are trying to figure out
#sample size so we leave this one blank.
#f2 is cohen f squared

##all the predictors
#x, m1, m2, cv1, x*m1, x*m2 = 6
pwr.f2.test(u = 6, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size

##addition of the interaction only
#x*m1 + x*m2
pwr.f2.test(u = 2, v = NULL, f2 = feta, sig.level = .05, power = .80)
#remember that you need to add u + v to get final sample size
