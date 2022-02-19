##Teil4
library(dplyr)
library(ggplot2)
library(DescTools)
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


Alter_st <- Daten$Alter[Daten$Studienfach=="Statistik"]
Alter_ds <- Daten$Alter[Daten$Studienfach=="Data Science"]
Alter_ma <- Daten$Alter[Daten$Studienfach=="Mathe"]
Alter_Info <- Daten$Alter[Daten$Studienfach=="Informatik"]

A(Daten$Alter)
#antwort
# mod 26
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.00   24.00   25.00   25.23   26.00   30.00 


A(Alter_ds)
#mod 25
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#21.00   24.00   25.00   24.97   26.00   30.00 

A(Alter_st)
#[1] 26
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#21.00   24.00   25.00   25.26   26.00   29.00 
A(Alter_Info)
#27
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#21.00   24.50   26.00   25.64   27.00   28.00 
A(Alter_ma)
#[1] 24
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#24.00   24.00   25.50   25.50   26.75   28.00 

#Alter
ggplot(data = Daten) + geom_boxplot(aes(x = Studienfach, y = Alter,  fill = Studienfach))

#Anzahl Studienfach
ggplot(data = Daten,aes(x =Studienfach ,fill=Studienfach))+
  geom_bar(stat = "count",position = "dodge")

#Anzahl Mathe_LK
ggplot(data = Daten,aes(x =Mathe_LK ,fill=Studienfach))+
  geom_bar(stat = "count",position = "fill")
