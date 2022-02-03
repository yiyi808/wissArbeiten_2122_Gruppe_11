## Aufgabenstellung 3.Teilleistung Gruppenarbeit/GitHub
## Teil 3
## Yi


# (a)
f1 <- function(x) summary(x)

f_1 <- function(x){
  for (i in 1:length(x)) {
  if(is.numeric(x[,i])) {
    print(summary(x[,i]))
    }
  }
}


# (b)
f2 <- function(x) summary(x)

f_2 <- function(x){
  for (i in 1:length(x)) {
    if(is.factor(x[,i])) {
      print(summary(x[,i]))
    }
  }
}

# (c)
f3 <- function(x, y) { 
  t <- table(x,y)
  return(chisq.test(t))
  }

# (d)
f4 <- function(x, y) cor.test(x, y, method=c("pearson"))

# (e)

# (f)
library(corrplot)
f6 <- function(x) 

# Die Daten wird nur verwendet, um zu prüfen, ob die Funktionen funktionieren.
set.seed(123)
dat <- iris
l <- factor(1:5, levels = 1:5, ordered = TRUE)
dat$Levels <- sample(l, 150 , replace=TRUE)
type <- c("A", "B", "C", "D") 
dat$Type <- sample(type, 150, replace=TRUE)
dat$AB <- ifelse(dat$Type == c("A", "B"), 1, 0)
head(dat)

# Überprüfen:
f1(dat)
f_1(dat)
f2(dat)
f_2(dat)
f3(dat[,5], dat[,6])
f4(dat[,1], dat[,8])
res <- dat[,c(5,6,5)]

