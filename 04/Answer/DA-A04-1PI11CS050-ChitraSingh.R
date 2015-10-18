path <- getwd()
setwd(path)
mydata <- read.csv("bank-data.csv")
population <- mydata$income
cat("mean :", mean(population), "\n")
cat("variance: ", var(population), "\n")
cat("Standard deviation: ", sd(population), "\n")
jpeg(filename="all_data.jpg")
hist(population)
dev.off()

for(i in c(5, 15, 30, 50))
{
	means <- c()
	for(j in (1:30))
	{
		samp <- sample(population, size=i)
		means <- append(means, mean(samp))
	}
	jpeg(filename=paste("sample_size", i, ".jpg"))	
	hist(means, main=paste("Histogram of samples of size ", i))
	dev.off()
	cat("Mean of ", i, " sample size means is: ", mean(means), "\n")
	cat("Standard deviation of ", i, " sample size means is: ", sd(means), "\n")
	cat("Standard deviation of ", i, " sample size means is: normalized ", sd(means)*sqrt(i), "\n")
}
