#filename: DA-A01-1PI11CS050-ChitraSingh.R


interpolateConstGr <- function(mydata) #interpolation for a constant growth rate
{
	newdata <- subset(mydata, FALSE)	#new data-frame to store data temporarily
	i <- 1
	n <- nrow(mydata)
	while(i < n)
	{
		diff <- mydata$year[i+1] - mydata$year[i]					#difference to check if two years are consecutive or not
		grRate <- (mydata$amount[i+1] / mydata$amount[i])^(1/diff) - 1 		#Calculating the growth rate for a duration of 'diff' years
		prevAmt <- mydata$amount[i]
		if(diff > 1)
		{
			d <- diff -1
			for(j in c(1 : d))
			{
				y = mydata$year[i] + j 
				newAmount <- (prevAmt * grRate ) + prevAmt			#Calculating amount for years missing
				prevAmt <- newAmount
				newdata <- rbind(newdata, data.frame(year=y, amount=newAmount))
			}
		}
		i <- i+1
	}
	mydata <- rbind(mydata, data.frame(newdata))
	mydata <- mydata[order(mydata$year),]
	return(mydata)
}

# Age Analysis
# BarPlot
plotAgeBar <- function()
{
	per <- c(35, 55 ,8, 2)
	l <- c("18-25", "18-35", "36-45", "45-60")
	barplot(per, main="Age analysis on on-line shoppers", col=rainbow(length(lbls)), names.arg=l, ylab="% of online shoppers", xlab="Age Group")
}

# Reluctance to shop online
# Pie Chart
plotRelPie <- function()
{
	per <- c(30, 20, 25, 15, 10)
	l <- c("30% : prefer researching \nproducts and services online", "20% : find delivery costs too high", "25% : fear sharing personal financial
	information online ", "15% : lack trust on whether products \nwould be delivered in good condition ", "10% : don't have a credit or debit card")
	pie(per, labels=l, main="Reluctance to shop online", col=rainbow(length(lbls)), density=90, radius=.5)
}


path <- getwd()
setwd(path)
mydata <- read.csv("DA-A01-1PI11CS050-ChitraSingh.csv")
data <- interpolateConstGr(mydata) 
print(data)

amt <- data$amount
year <- data$year
barplot(amt, main="Growth of India’s e-commerce market", col=rainbow(length(year)), names.arg=year, ylab="amount(in billion dollars)", xlab="years")
#plotAgeBar()
#plotRelPie()

#output for the given data
#year    amount
#2009  2.500000
#2010  3.759236
#2011  5.652744
#2012  8.500000
#2013 16.000000
#2014 18.135385
#2015 20.555763
#2016 23.299167
#2017 26.408711
#2018 29.933259
#2019 33.928199
#2020 38.456310
#2021 43.588750
#2022 49.406174
#2023 56.000000