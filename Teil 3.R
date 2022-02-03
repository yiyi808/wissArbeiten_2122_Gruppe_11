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