library("stringr")
library("distributions3")
source("functions.R")
library("sm")
T_9 <- StudentsT(df = 9)
summary.list = function(x)list(
  n<-length(x),
  N.with.NA.removed= length(x[!is.na(x)]),
  Count.of.NA= length(x[is.na(x)]),
  Mean=mean(x, na.rm=TRUE),
  Median=median(x, na.rm=TRUE),
  Mode=mode(x),
  Max.Min=range(x, na.rm=TRUE),
  Range=max(x, na.rm=TRUE) - min(x, na.rm=TRUE),
  Variance=var(x, na.rm=TRUE),
  
  Coeff.Variation.Prcnt=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100,
  Std.Error=sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])),
  Quantile=quantile(x, na.rm=TRUE),
  
  Mean=mean(x),
  minMeant=mean(x) + quantile(T_9, 0.12 / 2) * sd(x) / sqrt(n),
  maxMeant=mean(x) + quantile(T_9, 1 - 0.12 / 2) * sd(x) / sqrt(n),
  
  
  Std.Dev=sd(x, na.rm=TRUE),
  minStd=sd(x, na.rm=TRUE) + quantile(T_9, 0.12 / 2) * sd(x) / sqrt(n),
  maxStd=sd(x, na.rm=TRUE) + quantile(T_9, 1 - 0.12 / 2) * sd(x) / sqrt(n)
  
  
  
)

data1 = summary.list(distribution$data)
print(data1)



hist(distribution$data, breaks=13, col="red") 
d <- density(mtcars$mpg)
plot(d)



CDF <- ecdf(distribution$data )
plot( CDF )

sample_Data1 = rnorm(500)
sample_Data2 = rnorm(500)

test <- t.test(sample_Data1, sample_Data2,
               alternative = "greater",
               paired = TRUE
)

test


test <- wilcox.test(sample_Data1, sample_Data2, alternative = "two.sided")


test






