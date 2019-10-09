# Serial Mediation with Moderation on A1 Path

# Toy data
set.seed(123)
N <- 200
X <- rep(c("C", "T"), each = 100) # c = control vs. t = treatment
M1 <- c(rnorm(100, 1.17, .2), rnorm(100, 2.99, .2))
M2 <- 1.2*M1 + rnorm(N, 2.5, .2)
Y <- 0.2*M2 + rnorm(N, 2.5, .2)
W <- 0.7*M1 + rnorm(N, 2.5, .2)
data1 <- data.frame(X, M1, M2, W, Y)

# Install package
library(devtools)
install_github("doomlab/MeMoBootR")

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


y = "Y"
x = "X"
m1 = "M1"
m2 = "M2"
mod = "W"
cvs = NULL
df = data1
with_out = T
nboot = 1000
conf_level = .95
