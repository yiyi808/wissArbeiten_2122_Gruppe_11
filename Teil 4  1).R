
Daten <- read.csv("~/GitHub/wissArbeiten_2122_Gruppe_11/Daten.csv")
View(Daten)
str(Daten)
#Gibt es 6 Variablen und Jede Variable hat 100 Werte.
#Nummer jeder Person£¬Alter ,Studienfach ,Interesse_Math,Interesse_Prog,Mathe_LK
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

mod <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}


A <- function(x){
  print(mod(x))
  print(summary(x))
}
Alter<-Daten$Alter
A(Alter)
#[1] 26
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#21.00   24.00   25.00   25.23   26.00   30.00 
#Das bedeutet, dass die Personen in diesen Daten zwischen 21 und 30 Jahre alt sind. 
#26-Jaehrige sind die meisten.
boxplot( Alter,border = "red")




