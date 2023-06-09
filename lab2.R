options(digits=2)
# Створення таблиці
################################################################################
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
              "David Jones", "Janice Markhammer", "Cheryl Cushing",
              "Reuven Ytzrhak", "Greg Knox", "Joel England",
              "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
mydata <- data.frame(Student, Math, Science, English,
                     stringsAsFactors=FALSE)

# Підрахунок усередненого балу
################################################################################
scaled <- scale(mydata[,2:4])
score <- apply(scaled, 1, mean)
cbind(mydata, score)


# Підрахунок усередненого балу по ABCDF шкалі
################################################################################
y <- quantile(score, c(0.8, 0.6, 0.4, 0.2))
mydata$grade[score >= y[1]] <- "A"
mydata$grade[score < y[1] & score >= y[2]] <- "B"
mydata$grade[score < y[2] & score >= y[3]] <- "C"
mydata$grade[score < y[3] & score >= y[4]] <- "D"
mydata$grade[score < y[4]] <- "F"
mydata


# Ділення імені на складові
################################################################################
name <- strsplit(mydata$Student," ")
lastname <- sapply(name, "[", 2)
firstname <- sapply(name, "[", 1)
cbind(firstname, lastname, mydata[, -1])

# Сортування по імені (при однакових - по прізвищу)
################################################################################
mydata[order(firstname, lastname),]


# Сортування по прізвищу (при однакових - по імені)
################################################################################
mydata[order(lastname, firstname),]


# Сортування реверс по прізвищу (при однакових - по імені)
################################################################################
mydata[rev(order(lastname, firstname)),]
