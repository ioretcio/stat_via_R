library("stringr")
library("distributions3")
library("sm")


source("functions.R")


distribution <- read.delim("normal.txt") #зчитування даних

quartiles <- quantile(distribution$data, probs=c(.25, .75), na.rm = FALSE) #квантилі для відсікання аномальних
IQR <- IQR(distribution$data)  #довірчі межі між квантилями
Lower <- quartiles[1] - 1.5*IQR   # 
Upper <- quartiles[2] + 1.5*IQR   #
without_anomal <- subset(distribution$data, distribution$data > Lower & distribution$data < Upper)  #відсікання тих що не потрапили в довірчий інтервал
length(without_anomal)


T_9 <- StudentsT(df = 9) #ініціалізація Ст'юдента
summary.list = function(x)list(
  n<-length(x),   #довжина
  N.with.NA.removed= length(x[!is.na(x)]),  # кількість ненульових
  Count.of.NA= length(x[is.na(x)]),  #кількість нульових (NA)
  Mean=mean(x, na.rm=TRUE),  # середнє
  Median=median(x, na.rm=TRUE),  #медіана
  Mode=mode(x),  #...
  Max.Min=range(x, na.rm=TRUE),  #максимальне та мінімальне
  Range=max(x, na.rm=TRUE) - min(x, na.rm=TRUE),  #розкид даних
  Variance=var(x, na.rm=TRUE),   #дисперсія

  Quantile=quantile(x, na.rm=TRUE),  # детально по квантилям
  
  Mean=mean(x),  #середнє
  minMeant=mean(x) + quantile(T_9, 0.12 / 2) * sd(x) / sqrt(n),  # нижня довірча межа для середнього
  maxMeant=mean(x) + quantile(T_9, 1 - 0.12 / 2) * sd(x) / sqrt(n),  # верхня довірча межа для середнього
  
  
  Std.Dev=sd(x, na.rm=TRUE), #стандартне відхилення
  minStd=sd(x, na.rm=TRUE) + quantile(T_9, 0.12 / 2) * sd(x) / sqrt(n), # нижня довірча межа для стандартного відхилення
  maxStd=sd(x, na.rm=TRUE) + quantile(T_9, 1 - 0.12 / 2) * sd(x) / sqrt(n) # верхня довірча межа для стандартного відхилення
)

data1 = summary.list(without_anomal) #підрахунок всього що вище
print(data1)



hist(without_anomal, breaks=13, col="red")   # гістограма частот
d <- density(mtcars$mpg)  # графік частот
plot(d)



CDF <- ecdf(without_anomal)  #  кумулятивна функція розподілу 
plot( CDF )

sample_Data1 = rnorm(500)   #  набори даних для тестів
sample_Data2 = rnorm(500)   # можна і прочитати  distribution12345 <- read.delim("normal.txt")

test <- t.test(sample_Data1, sample_Data2,   #  ttest
               alternative = "greater",
               paired = TRUE
)

test


test <- wilcox.test(sample_Data1, sample_Data2, alternative = "two.sided")  #  вілкоксон


test



eval.parent(substitute( without_anomal <- without_anomal + 1)) # зсув +1



standarted <- scale(without_anomal) #стандартизація


standarted
mean(standarted)
sd(standarted, na.rm=TRUE)



hist(without_anomal, breaks=13, col="yellow")   # гістограма частот
logdata = log(without_anomal) # логарифмування
hist(logdata, breaks=13, col="green")   # гістограма частот



