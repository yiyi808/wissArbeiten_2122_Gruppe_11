##Teil4
library(dplyr)
library(ggplot2)
library(DescTools)
library(hrbrthemes)
library(viridis)
#Fuer die ganze Daten
Daten <- read.csv("~/GitHub/wissArbeiten_2122_Gruppe_11/Daten.csv")
View(Daten)
str(Daten)
#Es gibt 6 Variablen in diesem Dataframe und Jede Variable hat 100 Werte.
#6 Variablen sind: Nummer jeder Person, Alter, Studienfach, Interesse_Math, Interesse_Prog, Mathe_LK.
Alter<-Daten$Alter
Studienfach<-Daten$Studienfach 
#Es gibt vier Studienf??cher:Statisitk ,Data Science, Mathe,Informatik.
Interesse_Math<-Daten$Interesse_Math
#Interesse_Math haben Sieben Stufen: 1,2,3,4,5,6,7.
Interesse_Prog<-Daten$Interesse_Prog
#Interesse_Prog haben Sieben Stufen: 1,2,3,4,5,6,7.
Mathe_LK<-Daten$Mathe_LK
#Eine dichotome Variable mit 2 Level :
#1(bedeutet "ja"),0(bedeutet "Nein")

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
#47 Personen studieren Statistik.32 Personen studieren Data Science.
#10 Personen studieren Mathe.  11 Personen studieren Informatik.

A(Daten$Alter)
#antwort
# mod 26
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.00   24.00   25.00   25.23   26.00   30.00 
#Das bedeutet, dass die Personen in diesen Daten zwischen 21 und 30 Jahre alt sind. 
#26-Jaehrige sind die meisten.

A(Alter_ds)
#mod 25
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#21.00   24.00   25.00   24.97   26.00   30.00 

A(Alter_st)
#[1] mod 26
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#21.00   24.00   25.00   25.26   26.00   29.00 
A(Alter_Info)
#mod 27
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#21.00   24.50   26.00   25.64   27.00   28.00 
A(Alter_ma)
#[1]mod 24
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#24.00   24.00   25.50   25.50   26.75   28.00 


########Boxplot: Gesamte befragte Altersverteilung#######
boxplot( Alter,border = "red")
###Boxplot und Plot zwischen Alter & Studienfach ###