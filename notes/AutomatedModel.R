library(leaps)
library(dplyr)
nj <- read.csv("http://andrewpbray.github.io/data/crime-train.csv", na = c("", "NA", "?"))

train <- select(nj, population:PctSameState85,
                LandArea:PctUsePubTrans, ViolentCrimesPerPop)
mf <- regsubsets(ViolentCrimesPerPop ~ . , data = train, nvmax = 25, method = "forward")
plot(mf)
mb <- regsubsets(ViolentCrimesPerPop ~ . , data = train, nvmax = 25, method = "backward")
plot(mb)

input <- paste(names(coef(mf, 14))[-1], collapse=" + ")
m1 <- lm(ViolentCrimesPerPop~as.formula(names(coef(mf, 14))[-1]), data = nj)
