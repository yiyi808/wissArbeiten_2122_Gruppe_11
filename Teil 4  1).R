library(dplyr)
library(ggplot2)
library(DescTools)
library(data.table)
library(hrbrthemes)
library(gridExtra)
library(viridis)
library(extrafontdb)

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


#Boxplot und Plot zwischen Alter & Studienfach 
Daten%>%
  ggplot( aes(x = Studienfach, y = Alter,  fill = Studienfach)) +
  geom_boxplot() +
  xlab("Studienfach")+
  scale_fill_viridis(discrete = TRUE,option = "E" ,alpha=0.6) +
  ggtitle("Boxplots des Alters nach Studienfach ")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11) )
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
  ylab("Absolute Haufigkeit")+
  xlab("Studienfach")+
  scale_fill_viridis(discrete = T,alpha=0.6,option = "E") +
  ggtitle("Balkendiagramm  der Studentenzahlen in den vier Studienf??cher") +
  theme_ipsum()

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


#Das nachstehende Histogramm zeigt den prozentualen Anteil der Personen in jedem Studienfach,
#die in der schule Mathe-LK hatte oder nicht.
data <- Daten%>% group_by(Studienfach,Mathe_LK)%>%
  summarise(anzahl=sum(length(Mathe_LK))) 
data$Mathe_LK[data$Mathe_LK=="2"] <-"nein"
data$Mathe_LK[data$Mathe_LK=="1"] <- "ja"
ggplot(data,aes(x=Studienfach  ,fill=Mathe_LK,y=anzahl))+
  geom_bar(stat = "identity",position = "fill")+ylab("relative H??ufigkeit")+
  xlab("Studienfach")+
  scale_fill_viridis(discrete = T,option = "E",alpha=0.6) +
  ggtitle("") +
  theme_ipsum()
#Aus dieser Grafik geht klar hervor, dass neunzig Prozent der Personen, 
#die Statistik und Data Science studieren, keine Mathe-LK besucht haben. 
#Dagegen haben sechzig Prozent der Informatik- und Mathe_Studenten eine Mathe-LK besucht. 
#Dieser deutliche Unterschied l??sst darauf schlie??en, dass der Studienfach einen Einfluss darauf hat, ob jemand eine Mathe-LK  hatte oder nicht.


###### Mathe & Programmierung Interesse######  
#Unterteilen Sie das Interesse in drei Stufen: niedrig, mittel und hoch
intern <- function(x) min(x):max(x)
f5 <- function(x){
  Q <- quantile(intern(x), probs = c(1/3, 2/3), na.rm = TRUE)
  # Klasseneinteilung
  k<- numeric(length(x))
  k[x <= Q[2]] <- "niedrig"
  k[Q[1] <= x & x < Q[2]] <- "mittel"
  k[x >= Q[2]] <- "hoch"
  k[is.na(x)] <- NA
  return(k)
}

Daten$Skala_m <- f5(Daten$Interesse_Math)
Daten$Skala_p <- f5(Daten$Interesse_Prog)

#Diese Grafik zeigt, wie viele Studenten verschiedener studienfaecher sich f¨¹r das Programmieren von hoch nach niedrig interessieren.
ggplot(data = Daten,aes(x=Skala_p,fill=Studienfach))+
  geom_bar(stat = "count",position="dodge")+
  theme(axis.title.x=element_blank())+
  ylab("Anzahl")+
  xlab("Interesse Stufe")+
  scale_fill_viridis(discrete = T,alpha=0.6,option = "E") +
  ggtitle("Balkondiagramm des Interesse an der Programmierung  ") +
  theme_ipsum()


#Diese Grafik zeigt, wie viele Studenten verschiedener studienfaecher sich f¨¹r  Mathe von hoch nach niedrig interessieren.
ggplot(data = Daten,aes(x=Skala_m,fill=Studienfach))+
  geom_bar(stat = "count",position="dodge")+
  theme(axis.title.x=element_blank())+
  ylab("Anzahl")+
  xlab("Interesse Stufe")+
  scale_fill_viridis(discrete = T,alpha=0.6,option = "E") +
  ggtitle("Balkondiagramm des Interesses an der Mathe ") +
  theme_ipsum()


