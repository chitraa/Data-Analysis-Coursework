#File : DA-A02-1PI11CS050-ChitraSingh.R

#Reading data from the given file
path <- getwd()
setwd(path)
mydata <- read.csv("glass.csv")
#print(mydata)

#Table of mean and standard deviations
df <- data.frame(type=character(), mean=numeric(), sd=numeric())
for(n in names(mydata))
{
	if(is.numeric(mydata[[n]]))
	{
		df <- rbind(df, data.frame(type=n, mean=mean(mydata[[n]]), sd=sd(mydata[[n]])))
	}
}
print(df)

#generating boxplots
for(n in names(mydata))
if(is.numeric(mydata[[n]]))
{
	{
		print(n)
		jpeg(filename=n)
		boxplot(mydata[[n]]~mydata$Type,varwidth=TRUE)
		dev.off()
	}
}

#generating histograms (eg code)
nglass <- subset(glass,glass$Type %in% c("HL"))
for(n in names(nglass))
{
	if(is.numeric(mydata[[n]]))
	{
		jpeg(filename=n)
		hist(nglass[[n]])
		dev.off()
	}
}