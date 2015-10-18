mydata <- read.csv("bank-data.csv")
incomes <- mydata[[5]]
pSd <- sd(incomes)
pMean <-mean(incomes)

myCI <- function(sampleSize, CI, nSamples)
{
	count <- 0
	for(i in 1:nSamples)
	{
		X <- mean(sample(incomes, sampleSize))
		width <- -(qnorm((1-CI)/2) * pSd / sqrt(sampleSize))
		ul <- X + width
		ll <- X - width
		
		if(pMean < ll || pMean > ul)
		{
			count <- count + 1
		}
	}
	#p : proportion of samples where population mean lay within the confidence interval around sample mean
	p <- (nSamples - count) / nSamples
	cat("sample size: ", sampleSize, " CI: ", CI, " no of samples: ", nSamples, " count: ", count, " proportion: ", p, "\n")
	
	#jpeg(filename=paste(sampleSize, "_", nSamples, ".jpg"))
	#forestplot(labeltext=cbind(c(1:nSamples)), mean=m, lower=llList, upper=ulList, graphwidth=unit(6, "inches"), boxsize=.1)
	#dev.off()

}

for(j in c(100, 1000, 10000))
{
	myCI(30, .95, j)
}
for(j in c(100, 1000, 10000))
{
	myCI(60, .95, j)
}

#To check failure of CIs for various sample sizes, in 100 iterations of each
for(j in c(100, 1000, 10000))
{
	countWrong <- 0
	for(k in 1:100)
	{
		#p <- myCI(30, .95, j)
		p <- myCI(60, .95, j)
		#p <- myCI(90, .95, j)
		if(p < .95)
			countWrong <- countWrong + 1
	}
	cat("wrong p: ", countWrong, "\n")
}

#Plotted forestplots
#myCI(30, .95, 30)
#myCI(60, .95, 30)
#myCI(90, .95, 30)
