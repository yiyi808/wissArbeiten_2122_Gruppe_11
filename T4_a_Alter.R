##Teil4
#a)
#Alter

mod <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

A <- function(x){
  print(mod(x))
  print(summary(x))
}


A(Daten$Alter)
#antwort
# mod 26
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.00   24.00   25.00   25.23   26.00   30.00 

Alter<-Daten$Alter
Boxplot(Alter)
