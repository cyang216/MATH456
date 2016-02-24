## On Your Own
##### On Your Own

1. Using the family lung function data, relate FEV1 to heigh tfor the oldest
child in four ways: simple linear regression, regression of FEV1 on height
squared, spline regression with one knot at at height = 64, and natural
splines using the `smooth.spline` or `ns` function in R. 

library(ggplot2)
fev <- read.table("C:/Github/MATH456/data/Lung_020716.txt", sep="\t", header=TRUE)
names(fev) <- tolower(names(fev))
qplot(x=ocheight, y=ocfev1, data=fev, geom='point') + geom_smooth()

# SLR
slr <- lm(ocfev1 ~ ocheight, data=fev)

# MLR on ht^2
fev$ocheight.sq <- fev$ocheight^2
mlr <- lm(ocfev1 ~ ocheight + ocheight.sq, data=fev)

# knot
fev$gt64 <- ifelse(fev$ocheight>64, 1, 0)
splne <- lm(ocfev1 ~ gt64 + ocheight + gt64*ocheight, data=fev)

ggplot(fev, aes(x=ocheight, y=ocfev1)) + geom_point() + geom_smooth(se=FALSE) +
    geom_smooth(data=subset(fev, gt64==0), method="lm", col="red", se=FALSE) + 
    geom_smooth(data=subset(fev, gt64==1), method="lm", col="red", se=FALSE)

# natural splines
fit.ns <- lm(ocfev1 ~ ns(ocheight, 3), data=fev )

# Smoothing splines
fit.sp = smooth.spline(fev$ocfev1 ~ fev$ocheight,  nknots=15)


2. Using the $R^{2}$ value and a test of overall fit, which model fits the 
data the best? 





3. On each of the four models, calculate a confidence interval for the
predicted average FEV1 for a child who's height is 60cm and 64cm. 
This is 8 prediction intervals in total. Comment on the
differences in prediction intervals across the two heights, and 
across the four models. 
