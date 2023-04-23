custom_read_file <-function(path)
{
  read.delim(path) #зчитування даних
}

costum_anomal_deleting <- function(distribution)
{
  quartiles <- quantile(distribution, probs=c(.25, .75), na.rm = FALSE) #квантилі для відсікання аномальних
  IQR <- IQR(distribution)  #довірчі межі між квантилями
  Lower <- quartiles[1] - 1.5*IQR   # 
  Upper <- quartiles[2] + 1.5*IQR   #
  subset(distribution, distribution > Lower & distribution < Upper)  #відсікання тих що не потрапили в довірчий інтервал
}

costum_nulls_deleting <- function(distribution)
{
  distribution[!is.na(distribution)]
}

{
custom_stats_calculate <- function(x)list(
    T_9 <- StudentsT(df = 9), #ініціалізація Ст'юдента
    n=length(x),   #довжина
    n<-length(x),   #довжина
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
}

custom_mode <- function(distribution)
{
  uniqv <- unique(distribution)
  uniqv[which.max(tabulate(match(distribution, uniqv)))]
}

custom_shift <- function(distribution, value)
{
  eval.parent(substitute( distribution <- distribution + value))
}

custom_density_chart <- function(distribution, color)
{
  hist(distribution, breaks=13, col=color)   # гістограма частот
}

custom_cumulative_distribution_chart <- function(distribution)
{
  CDF <- ecdf(distribution)  #  кумулятивна функція розподілу 
  plot( CDF )
}


custom_standartization <- function(distribution)
{
  scale(distribution)
}


custom_logdata <- function(distribution)
{
  log(distribution)
}

custom_ttest <-function(distribution1, distribution2)
{
  t.test(distribution1, distribution2,   #  ttest
         alternative = "greater",
         paired = TRUE
  )
}

custom_wilcoxon <-function(distribution1, distribution2)
{
  wilcox.test(distribution1, distribution2, alternative = "two.sided")  #  вілкоксон
}