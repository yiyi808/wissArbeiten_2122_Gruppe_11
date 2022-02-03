# Haiyi Rong 
#a)
summary()

library(ggplot2)
A <- function(x){
  describe(x) 
}

#b)
B <- function(x){
  x <- as.factor(x)
  return(x)
}


#c)
library(DescTools)
C <- function(x,y){
  tab <- table(x,y)
  Assocs(tab)
  chisq.test(x,y)
}

#d)
library(Hmisc) 

D <- function(x){
  Hmisc::rcorr(as.matrix(x), type="pearson") 
}

#f)
ggplot()


# Yi Sun 


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

#Li Jinzhen

#Schritt

f1<- function(x, na.omit=FALSE){ 
  if (na.omit) 
    x <- x[!is.na(x)] 
  m <- mean(x) 
  n <- length(x) 
  s <- sd(x) 
  var <- var(x)
  min <- min(x)
  max <- max(x)
  q<- quantile(x)
  Tukey <- fivenum(x)
  md <- median(x)
  skew <- sum((x-m)^3/s^3)/n 
  kurt <- sum((x-m)^4/s^4)/n - 3 
  mod <- function(x) {
    uniqv <- unique(x)
    uniqv[which.max(tabulate(match(x, uniqv)))]
  }
  
  return(c(n=n, mean=m,var=var,s=s, skew=skew, kurtosis=kurt,min=min,max=max,quantile=q,median=md,modul=mod(x))) } 

f2 <- function(x){
 x <- as.factor(x)
  summary(x)
   return(x)
}

f3 <- function(x,y){
  k <- table(x,y)
  chi <- chisq.test(k)$statistic
  
}
#x sind metrischen Variablen  y ist dichotomen Variablen
#Biserial
f4 <- function(x,y){
  y[y=="ja"] <- 1
  y[y=="nein"] <- -1
  cor.test(x, y)
}

f5 <- function(x){
  Q <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
  # Klasseneinteilung
  k<- numeric(length(x))
  k[x <= Q[2]] <- "niedrig"
  k[Q[1] <= x & x < Q[2]] <- "mittel"
  k[x >= Q[2]] <- "hoch"
  k[is.na(x)] <- NA
  Tafel <- table(k, x)
  Tafel}


#Xinyi  Jing
#Aufgabe 3
##a)
A <- function(x){
  summary(x) 
}
#order
library(psych)
A <- function(x){
  describe(x) 
}

##b)
B<-function(x){factor(x = character(), levels, labels = levels, ordered = is.ordered(x))
 return(summary(x))}

##c)

C<-function(x1,x2){
  t<-table(x1,x2)
  return(chisq.test(t))}
 

##d)
#Cochran-Mantel-Haenszel Test
A <- function(x){
  cmh.test(x1,x2) 
}



##e)
E<-function(x){y<- factor(x, order = TRUE, levels =c('niedrig', 'mittel', 'hoch'))
summary(y)}

##f)
boxplot()
hist()
barplot()
ggplot()



###################################
###################################
# str()
#a)&b)

A <- function(x){
  summary()
}


#c)
library(DescTools)
C <- function(x,y){
  tab <- table(x,y)
  Assocs(tab)
  chisq.test(x,y)
}

#d)
D <- function(x, y){
  cor.test(x, y, method=c("pearson"))
}

#e)


#f)
