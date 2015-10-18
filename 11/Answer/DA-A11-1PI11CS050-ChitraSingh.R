require(graphics)

path <- getwd()
setwd(path)
mydata <- read.csv("head-brain.csv")

# 1 : scatter plot of the data
jpeg(filename=paste("scatterplot", ".jpg"))
y <- plot(mydata)
l <- lm(mydata$brain~mydata$head, data=mydata)
#l <- lm(mydata)
abline(l)
dev.off()

#2 : linear model to predict brain weight from the head volume using the data
cat("\n 2. ---linear model to predict brain weight from the head volume using the data ----\n")
print(summary(l))

#3 : 95% confidence for the values of the regression coefficients
c <- confint(l)
cat("\n 3. ---95% confidence for the values of the regression coefficients ----\n")
print(c)

#4 : plot of the residuals as a function of the fitted values
y <- predict.lm(l, interval="confidence", level=.95)
jpeg(filename=paste("residuals", ".jpg"))
f <-fitted(l)
plot(f, l$residuals, xlab="fitted values(brain weight)") 
dev.off()

#5 : 95% confidence interval for the predicted brain weight of an individual whose head volume is 4000 cc
x <- 4000
y <- coefficients(l)[1] + coefficients(l)[2]*x
n <- length(mydata$head)
syhat <-(1/n)+(((x-mean(mydata$head))^2)/((n-1)*var(mydata$head)))
r <- cor(mydata)[2]
s <- (1-r^2)*(n-1)*(var(mydata$brain))/(n-2)
sy <- sqrt(syhat) * s
t <- qt(0.975,n-2)
low <- y - (t*sy)
up <- y + (t*sy)
cat("5. ---95% confidence interval for the predicted brain weight of an individual whose head volume is 4000 cc
---\n")
cat("fitted brain value: ", y, "\n")
cat("lower interval: ", low, "\n")
cat("upper interval: ", up, "\n")
