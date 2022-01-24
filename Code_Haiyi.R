### Auf_2 ######  Haiyi Rong ####

## Alter, Studienfach, Interesse an Mathematik, Interesse an Programmieren, 
## Math-LK(ja/nein)

set.seed(1234)

##1)
Alter <- c(rnorm(100, mean = 25, sd = 2))

##2)
# S:Staistik  D:Data Science M:Mathe I:Informatik
SF <- c(sample(c("S", "D", "M", "I"), 100, replace=TRUE, prob=c(0.35, 0.35, 0.2, 0.1)))


##3)
IM <- c(sample(rep(1:7,1), 100, replace=TRUE))
data <- data_frame(Alter, SF, IM)
# Mathematik 
# Die meisten Menschen, die Mathematik studieren, interessieren sich dafür, daher liegt der Wert bei 6,7
data$IM[data$SF == c("M")] <- c(sample(c(6,7), length(data$IM[data$SF == c("M")]), replace=TRUE))
data

##4)
IP <- sample(rep(1:7,1), 100, replace=TRUE)
data <- cbind(data, IP)
# Informatik 
# Die meisten Menschen, die Informatik studieren, interessieren sich dafür, daher liegt der Wert bei 6,7
data$IP[data$SF == c("I")] <- c(sample(c(6,7), length(data$IP[data$SF == c("I")]), replace=TRUE))
# Data Science 
# Die meisten Menschen, die Data Scuence studieren, interessieren sich dafür, daher liegt der Wert bei 4,5,6,7
data$IP[data$SF == c("D")] <- c(sample(c(4,5,6,7), length(data$IP[data$SF == c("D")]), replace=TRUE))

##5)
MK <- sample(c("Ja", "Nein"), 100, replace=TRUE)
data <- cbind(data, MK)
# Zusammenhänge mit dem Fach Mathematik
data$MK[data$SF == c("M")] <- c(sample(c("Ja", "Nein"), length(data$MK[data$SF == c("M")]), replace=TRUE, prob=c(0.8, 0.2)))
# data$MK[data$SF == c("M")] <- c(rep("Ja",length(data$MK[data$SF == c("M")])))

# Zusammenhang mit Interesse an Mathematik
data$MK[data$IM == 7] <- c(rep("Ja",length(data$MK[data$IM == 7])))

# Zusammenhang mit Programmierungsinteressen
data$MK[data$IP == 7] <- c(rep("Nein",length(data$MK[data$IP == 7])))

data

##### Def von LK ist nicht sicher

