#rnorm function can generate random numbers whose distribution is normal with specified mean and standard deviation

for(sampleSize in c(3,5,10,30,50,100))
{
	variances <- vector(mode="numeric", length=100)
	for(i in 1:100)
	{
		j <- var(rnorm(sampleSize, mean=0, sd=1))	#sample variance
		variances[i] <- j*(sampleSize - 1)	#chi squared distribution with (sampleSize - 1) degrees of freedom
	}
	jpeg(filename=paste(sampleSize, ".jpg"))
	y <- hist(variances)
	#print(max(y$counts))
	x <- variances
	curve(dchisq(x,df=sampleSize-1)*max(y$counts), col='blue', add=TRUE )		#multiplied by the max frequency (max(y$counts)) to make the chi squared curve visible
	dev.off()
}
