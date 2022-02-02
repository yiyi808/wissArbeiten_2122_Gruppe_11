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



