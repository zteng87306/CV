library(ggplot2)
library(GGally)
library(MASS)
library(car)
library(plyr)
library(tidyverse)
library(effects)
library(alr4)
library(readxl)
library(ggfortify)
data <- read_excel("/Users/zhaoliteng/Downloads/data1.xlsx")
data1 <- data[,c(-1:-4)]
data2 <- data1[-1,]
data2 = as.data.frame(data2)
data2$DEAD_CNT= as.numeric(as.character(data2$DEAD_CNT))
data2$`03_CNT` = as.numeric(as.character(data2$`03_CNT`))
data2$H_CNT = as.numeric(as.character(data2$H_CNT))
data2$H_SRVP = as.numeric(as.character(data2$H_SRVP))
data2$BORN_CNT = as.numeric(as.character(data2$BORN_CNT))
data2$COLUMN1 = as.numeric(as.character(data2$COLUMN1))
data2$E04_CNT = as.numeric(as.character(data2$E04_CNT))
data2$SOCIAL_INC_PER = as.numeric(as.character(data2$SOCIAL_INC_PER))
data2$MARRY_PER = as.numeric(as.character(data2$MARRY_PER))
data2$DIVORCE_PER = as.numeric(as.character(data2$DIVORCE_PER))
data2$E1112_CNT = as.numeric(as.character(data2$E1112_CNT))
data2$BORN_SEX_PER = as.numeric(as.character(data2$BORN_SEX_PER))

y = data2$DEAD_CNT
x1 = data2$`03_CNT`
x2= data2$H_CNT
x3 = data2$H_SRVP 
x4 = data2$BORN_CNT 
x5 = data2$COLUMN1 
x6 =data2$E04_CNT 
x7=data2$SOCIAL_INC_PER 
x8=data2$MARRY_PER 
x9=data2$DIVORCE_PER 
x10=data2$E1112_CNT 
x11=data2$BORN_SEX_PER


str(data2)
boxplot(data2$DEAD_CNT)
hist(data2$DEAD_CNT)
hist(log(data2$DEAD_CNT))
ggpairs(data2)

r1 <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11,data = data2)
vif(r1)
r0 <- lm(y~x2+x3+x4+x5+x6+x7+x8+x9+x10+x11,data = data2)
vif(r0)
# Step Forward #
nullmodel <- lm(y~1, data = data2)
forward <- step(nullmodel, scope = list(lower = nullmodel, upper = r0),
                direction = "forward")
summary(forward)
# Step Backward #
backward <- step(r0, scope = list(upper = r0),direction = "backward")
summary(backward)
# Step Both #
stepwise <- stepAIC(r0, direction = "both")
summary(stepwise)
##################
r5 <- lm(y~x3+x4+x5+x6+x7+x10,data = data2)
r6 <- lm(log(y)~x3+log(x4)+log(x5)+log(x6)+x7+log(x10),data = data2)
shapiro.test(resid(r6))
qqnorm(resid(r6))
qqline(resid(r6))


boxcox <- boxcox(r5)

lambda <- boxcox$x[which.max(boxcox$y)]

y <- ((y^lambda)-1)/lambda
x <- data2[,c(4,5,6,7,8,11)]
powerTransform(x,family = "yjPower")



r6 <- lm(y~sqrt(x3)+log(x4)+log(x5)+sqrt(x6)+x7+log(x10),data = data2)
plot(r6)
shapiro.test(resid(r6))
residualPlots(r6)
ks.test(scale(r6$residuals),pnorm)
summary(r6)
durbinWatsonTest(r6)
ncvTest(r6)
r7 <- lm(y~x3+log(x4)+log(x5)+log(x6)+x7+log(x10)+x3*log(x4)+x3*log(x5)
         +x3*log(x6)+x3*x7+x3*log(x10)+log(x4)*log(x5)
         +log(x4)*log(x6)+log(x4)*x7+log(x4)*log(x10)+
         log(x5)*log(x6)+log(x5)*x7+log(x5)*log(x10)+
         log(x6)*x7+log(x6)*log(x10)+x7*log(x10),data = data2)
# Step Forward #
nullmodel <- lm(y~1, data = data2)
forward <- step(nullmodel, scope = list(lower = nullmodel, upper = r7),
                direction = "forward")
summary(forward)
# Step Backward #
backward <- step(r7, scope = list(upper = r7),direction = "backward")
summary(backward)
# Step Both #
stepwise <- stepAIC(r7, direction = "both")
summary(stepwise)
r8 <- lm(formula = y ~ x3 + log(x4) + log(x5) + log(x6) + x7 + log(x10) + 
     x3:log(x5) + x3:x7 + log(x4):log(x6) + log(x4):log(x10) + 
     log(x6):log(x10), data = data2)
shapiro.test(resid(r8))
vif(r8)
r9 <- lm(formula = y ~ x3 + log(x4) + log(x5) + log(x6) + x7 + log(x10) + 
           x3:log(x5) + x3:x7 + log(x4):log(x10) + 
           log(x6):log(x10), data = data2)
vif(r9)
r10 <- lm(formula = y ~ x3 + log(x4) + log(x5) + log(x6) + x7 + log(x10) + 
           x3:log(x5) + x3:x7 + log(x4):log(x10) , data = data2)
vif(r10)

r11 <- lm(formula = y ~ x3 + log(x4) + log(x5) + log(x6) + x7 + log(x10) + 
            x3:log(x5) + x3:x7  , data = data2)
vif(r11)

r12 <- lm(formula = y ~ x3 + log(x4) + log(x5) + log(x6) + x7 + log(x10) + 
             x3:x7  , data = data2)
vif(r12)
summary(r12)
shapiro.test(resid(r12))
ncvTest(r12)
durbinWatsonTest(r12)
plot(r12)

