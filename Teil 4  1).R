
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

#Boxplot: Gesamte befragte Altersverteilung
boxplot( Alter,border = "red")


#Boxplot zwischen Alter & Studienfach 
ggplot(data = Daten) + geom_boxplot(aes(x = Studienfach, y = Alter,  fill = Studienfach))
#Insgesamt glaube ich nicht, dass es einen gro??en Unterschied in der Altersverteilung der vier Studienf??cher gibt. 
#Das erste Quartil, der Median und das dritte Quartil liegen alle sehr nahe beieinander.
# Also anhand des Boxplots vermute ich, dass sich Alter und Studienfach nicht gegenseitig beeinflussen.


Alter_st <- Daten$Alter[Daten$Studienfach=="Statistik"]
Alter_ds <- Daten$Alter[Daten$Studienfach=="Data Science"]
Alter_ma <- Daten$Alter[Daten$Studienfach=="Mathe"]
Alter_Info <- Daten$Alter[Daten$Studienfach=="Informatik"]
#47 Personen studieren Statistik.32 Personen studieren Data Science.
#10 Personen studieren Mathe.  11 Personen studieren Informatik.
#Die Anzahl der Personen in jedem Studienfach wird in einem Balkendiagramm dargestellt.
ggplot(data = Daten,aes(x =Studienfach ,fill=Studienfach))+
  geom_bar(stat = "count",position = "dodge")+
ylab("Absolute Haufigkeit")

Daten$Mathe_LK[Daten$Mathe_LK=="0"] <- 2
ja<-Daten$Mathe_LK[Daten$Mathe_LK=="1"]
Nein<-Daten$Mathe_LK[Daten$Mathe_LK=="2"]
#20 Personen hatte in der Schule Mathe-LK.
#80 Personen hatte in der schule Mathe-LK.
#Die Anzahl der Personen wird in einem Balkendiagramm dargestellt
ggplot(data = Daten,aes(x =Mathe_LK ,fill=Mathe_LK))+
  geom_bar(stat = "count",position = "dodge")+
  xlim("ja","Nein")+
  ylab("Absolute Haufigkeit")

#Hier ist eine Histogramm des Anteils der vier studienf??cher an den beiden Wahlm??glichkeiten, 
#ob sie einen Mathe_LK belegt haben oder nicht.
ggplot(data = Daten,aes(x =Mathe_LK ,fill=Studienfach))+
  geom_bar(stat = "count",position = "fill")+
  xlim("ja","Nein")+
  ylab("relative Haufigkeit")
#Nachdem wir uns die Grafik angesehen haben, haben wir festgestellt, dass unter den Personen, die Mathe_LK genommen haben, 
#viele Personen Statistiken und Informationen studiert haben und der Gesamtanteil etwa 75 % betr??gt. Jeder machte etwa 35 % aus. 
#Ein sehr gro??er Teil der Menschen, die keinen Mathe_LK  besucht haben, studieren Data Science and Mathe. 
#Zusammen machen sie mehr als 90 Prozent aus.

