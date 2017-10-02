## COURSE PROJECT
## OMITTED IN KNITR
allmodel <- lm(mpg ~ .,data=mtcars)
idealmodel <- step(allmodel, direction = "both")  ##Select best model


data(mtcars)
mtcars$cyl  <- factor(mtcars$cyl)  ## Convert to factors for analysis
mtcars$engine <- factor(mtcars$vs,labels=c("V","S"))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am   <- factor(mtcars$am,labels = c("auto","man"))
##subtract columns
library(dplyr)
mtcars <- select(mtcars,-(qsec:vs))


linearmodel <- lm(mpg ~ factor(am),data=mtcars)
summary(linearmodel)$coef
allmodel <- lm(mpg ~ .,data=mtcars)
idealmodel <- step(allmodel, direction = "both")  ##Select best model
summary(idealmodel)
anova(linearmodel, idealmodel)
par(mfrow=c(2, 2))
plot(idealmodel)

pairs(mpg ~ ., data = mtcars)  ## pairs plot for correlation
library(gclus)
cpairs(mtcars,upper.panel=panel.smooth)
## create boxplot of data for manual vs transmission
library(cowplot)
a <- ggplot(mtcars,aes(x=am,y=mpg)) +geom_boxplot(fill=c("light blue","green"))+geom_point()+
      labs(title="Transmission Type", y= "Miles Per Gallon(MPG)")
b <- ggplot(mtcars,aes(x=cyl,y=mpg)) +geom_boxplot(fill=c("light blue","green","red"))+geom_point()+
      labs(title="# of Cylinders")
c <- ggplot(mtcars,aes(x=vs,y=mpg)) +geom_boxplot(fill=c("light blue","green"))+geom_point()+
      labs(title="Engine")
d <- ggplot(mtcars,aes(x=gear,y=mpg)) +geom_boxplot(fill=c("light blue","green","red"))+geom_point()+
      labs(title="# Gears")
e <- ggplot(mtcars,aes(x=wt,y=mpg)) +geom_point()+geom_point()+labs(title="Weight(Tons)")
plot_grid(a,b,c,d,e,ncol=5)


par(mfrow=c(5, 1))

library(GGally)
ggpairs(mtcars,lower=list(continuous="smooth",
        