library("stringr")
library("distributions3")
library("sm")

source("functions.R")

distribution <- custom_read_file("normal.txt")$data
distribution <- costum_anomal_deleting(distribution)


statistics = custom_stats_calculate(distribution)
statistics


statistics["Mean"]#demo
custom_shift(distribution, 1)
statistics = custom_stats_calculate(distribution)
statistics["Mean"] #demo


custom_density_chart(distribution, "red")
custom_cumulative_distribution_chart(distribution)



distribution <- custom_standartization(distribution)
statistics = custom_stats_calculate(distribution)
statistics["Mean"]#demo
statistics["Std.Dev"]#demo


custom_density_chart(distribution, "yellow")
distribution <-custom_logdata(distribution)
custom_density_chart(distribution, "orange")



sample_Data1 = rnorm(500)   #  набори даних для тестів
sample_Data2 = rnorm(500)   # можна і прочитати з файлу
custom_ttest(sample_Data1, sample_Data2)
custom_wilcoxon(sample_Data1, sample_Data2)