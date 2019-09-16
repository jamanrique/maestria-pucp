library(car)

scatterplotMatrix( ~ prestige + education + income, id=list (n=3),
                   data=Duncan)

scatter3d(prestige ~ income + education, data=Duncan)

M1=lm(prestige~education+income,data=Duncan)
summary(M1)

qqPlot(M1,id=list(n=3))

residualPlots(M1,~income+education,type="rstudent")

spreadLevelPlot(M1)

influenceIndexPlot(M1, vars=c ("Cook", "hat"), id=list(n=3))

avPlots(M1)

crPlots(M1)


M1.1=lm(prestige~education+income,data=Duncan,subset=-c(6,16))

compareCoefs(M1,M1.1)

summary(M1.1)
#####################################################

scatterplot (prestige ~ income | type,data=Prestige)

scatterplotMatrix (~ prestige + income + education + women,
                   data=Prestige)

scatterplotMatrix (
  ~ prestige + log (income) + education + women | type,
  data=Prestige)


M1=lm(prestige~education+log2(income)+women,data=Prestige)
summary(M1)

library ("effects")
plot (predictorEffects (M1))

M2=lm(prestige~education+log2(income)+type,data=Prestige)
summary(M2)

plot (predictorEffects (M2, predictors = ~ type + education +
                          income))



M3=lm(prestige~education*type+log2(income)*type,data=Prestige)
summary(M3)


plot (predictorEffects (M3, ~ education))
plot (predictorEffects (M3, ~ income))

Confint(M2)
confidenceEllipse (M2)

anova(M2,M3)

######################################################

plot (weight ~ repwt, data=Davis)
M=lm(weight ~ repwt,data=Davis,subset=-12)
linearHypothesis(M,diag(2),c(0,1))
linearHypothesis (M, c ("(Intercept) = 0", "repwt = 1"))

###################################

r=lm(mpg~.,data=mtcars)
library(MASS)
step(r)
stepAIC(r)

library(glmnet)
a=glmnet(as.matrix(mtcars[,-1]),as.matrix(mtcars[,1]))
plot(a)

b=cv.glmnet(as.matrix(mtcars[,-1]),as.matrix(mtcars[,1]))
plot(b)

coef(b,s="lambda.min")
coef(b,s="lambda.1se")
