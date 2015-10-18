require(graphics)

path <- getwd()
setwd(path)
mydata <- read.csv("glass.csv")

count <- 0
f <-function(nth)
{
	for(j in ((nth+1):9))
	{
		myCor <- cor(mydata[nth], mydata[j])
		myCorTest <- cor.test(mydata[[nth]], mydata[[j]], alternative="two.sided", conf.level=0.95)
		if(myCorTest$p.value <= 0.05)	#reject h0 (r=0; where r is population correlation) hence if(r is significantly different from 0)
		{
			jpeg(filename=paste(names(mydata)[nth],names(mydata)[j], myCorTest$estimate, ".jpg"))
			y<-plot(mydata[[nth]], mydata[[j]], xlab=names(mydata)[nth], ylab=names(mydata)[j])
			cat(names(mydata)[nth], names(mydata)[j], "p value: ", myCorTest$p.value, "r: ", myCorTest$estimate, "\n")
			dev.off()
		}
		count <<- count + 1
	}
}
cat("pairs which are significantly different from 0 are: \n")
for(i in (1:8))
{
	f(i)
}
cat(count, "\n")