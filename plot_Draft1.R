##Teil4
library(dplyr)
library(ggplot2)
library(DescTools)
library(hrbrthemes)
library(viridis)
library(vcd)

#Fuer die ganze Daten
Daten <- read.csv("~/GitHub/wissArbeiten_2122_Gruppe_11/Daten.csv")
View(Daten)
str(Daten)
#Es gibt 6 Variablen in diesem Dataframe und Jede Variable hat 100 Werte.
#6 Variablen sind: Nummer jeder Person, Alter, Studienfach, Interesse_Math, Interesse_Prog, Mathe_LK.
Alter<-Daten$Alter
Studienfach<-Daten$Studienfach 
#Es gibt vier Studienfächer:Statisitk ,Data Science, Mathe,Informatik.
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
#Insgesamt glaube ich , dass es keinen großen Unterschied in der Altersverteilung der vier Studienfächer gibt. 
#Das erste Quartil, der Median und das dritte Quartil liegen alle sehr nahe beieinander.
# Also anhand des Boxplots vermute ich, dass sich Alter und Studienfach nicht gegenseitig beeinflussen.

#b)Studienfach
B <- function(x){
  a <- levels(x)
  sum <- table(x)
  h <- prop.table(table(x))
  return(c(Art=a,Anzahl=sum,haeufig=h))
  }
B(Studienfach)
#Anzahl.Data Science    Anzahl.Informatik 
#               32.00                11.00 
#        Anzahl.Mathe     Anzahl.Statistik 
#               10.00                47.00 
#haeufig.Data Science   haeufig.Informatik 
#                0.32                 0.11 
#       haeufig.Mathe    haeufig.Statistik 
#                0.10                 0.47 

#47 Personen studieren Statistik.32 Personen studieren Data Science.
#10 Personen studieren Mathe.  11 Personen studieren Informatik.
#Die Anzahl der Personen in jedem Studienfach wird in einem Balkendiagramm dargestellt.

ggplot(data = Daten,aes(x =Studienfach ,fill=Studienfach))+
  geom_bar(stat = "count",position = "dodge")+
  ylab("Absolute Haufigkeit")+
  xlab("Studienfach")+
  scale_fill_viridis(discrete = T,alpha=0.6,option = "E") +
  ggtitle("Balkendiagramm  der Studentenzahlen in den vier Studienfaecher") +
  theme_ipsum()



#c)Mathe_LK
Daten$Mathe_LK[Daten$Mathe_LK=="1"] <- "ja"
Daten$Mathe_LK[Daten$Mathe_LK=="0"] <- "nein"
B(Mathe_LK)
 #Anzahl.0  Anzahl.1 haeufig.0 haeufig.1 
  #   80.0      20.0       0.8       0.2 
#20 Personen hatte in der Schule Mathe-LK.
#80 Personen hatte in der schule Mathe-LK.
#Die Anzahl der Personen wird in einem Balkendiagramm dargestellt
ggplot(data = Daten,aes(x =Mathe_LK ,fill=Mathe_LK))+
  geom_bar(stat = "count",position = "dodge")+
  xlim("ja","Nein")+
  ylab("Absolute Haufigkeit")+
 scale_fill_viridis(discrete = T,alpha=0.6,option = "E") +
  theme_ipsum()


#d)Interesse_Math 
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

#Diese Grafik zeigt, wie viele Studenten verschiedener studienfaecher sich für  Mathe von hoch nach niedrig interessieren.
ggplot(data = Daten,aes(x=Skala_m,fill=Studienfach))+
  geom_bar(stat = "count",position="dodge")+
  theme(axis.title.x=element_blank())+
  ylab("Anzahl")+
  xlab("Interesse Stufe")+
  scale_fill_viridis(discrete = T,alpha=0.6,option = "E") +
  ggtitle("Balkondiagramm des Interesses an der Mathe") +
  theme_ipsum()

e)Interesse_Prog
Daten$Skala_p <- f5(Daten$Interesse_Prog)
#Diese Grafik zeigt, wie viele Studenten verschiedener studienfaecher sich für das Programmieren von hoch nach niedrig interessieren.
ggplot(data = Daten,aes(x=Skala_p,fill=Studienfach))+
  geom_bar(stat = "count",position="dodge")+
  theme(axis.title.x=element_blank())+
  ylab("Anzahl")+
  xlab("Interesse Stufe")+
scale_fill_viridis(discrete = T,alpha=0.6,option = "E") +
  ggtitle("Balkondiagramm des Interesse an der Programmierung") +
  theme_ipsum()


#########Studienfach  &  Mathe_LK###############
#####Test zwischen Studienfach  &  Mathe_LK
C <- function(x,y){
  tab <- table(x,y)
  Assocs(tab)
  
  chisq.test(x,y)
}

C(Studienfach,Mathe_LK)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 43.967, df = 3, p-value = 1.534e-09
#Da p-value kleiner als 0.05 ist,
#gibt es einen signifikanten Zusammenhang zwischen Studienfach
#und Math_LK, d.h. die Studienfach ist nicht unabhaengig vom Math_LK.

#####Grafik zwischen Studienfach  &  Mathe_LK
#Das nachstehende Histogramm zeigt den prozentualen Anteil der Personen in jedem Studienfach,
#die in der schule Mathe-LK hatte oder nicht.
data <- Daten%>% group_by(Studienfach,Mathe_LK)%>%
  summarise(anzahl=sum(length(Mathe_LK))) 
data$Mathe_LK[data$Mathe_LK=="0"] <-"nein"
data$Mathe_LK[data$Mathe_LK=="1"] <- "ja"
ggplot(data,aes(x=Studienfach  ,fill=Mathe_LK,y=anzahl))+
  geom_bar(stat = "identity",position = "fill")+ylab("relative Häufigkeit")+
  xlab("")+
  scale_fill_viridis(discrete = T,option = "E",alpha=0.6) +
  ggtitle("") +
  theme_ipsum()
#Aus dieser Grafik geht klar hervor, dass neunzig Prozent der Personen, 
#die Statistik und Data Science studieren, keine Mathe-LK besucht haben. 
#Dagegen haben sechzig Prozent der Informatik- und Mathe_Studenten eine Mathe-LK besucht. 
#Dieser deutliche Unterschied laesst darauf schließen, dass der Studienfach einen Einfluss darauf hat, ob jemand eine Mathe-LK  hatte oder nicht.
#Das heißt, von denjenigen, die Mathe und Informatik als Studienfach haben, haben die meisten von ihnen Math_LK in der Schule besucht. 
#Und die Leute, die Statistik und Data Science studieren, haben meistens keinen Math_LK in der Schule besucht.



#Grafik zwischen Studienfach & Interesse_Math & Mathe_LK

#mosaicplot zwischen Studienfach & Interesse_Math & Mathe_LK
Mosaicplot_Math<-mosaic(~Studienfach+Skala_m+Mathe_LK,data=Daten,highlighting = 'Mathe_LK', highlighting_fill=c('#FAFAD2','#DCDCDC'))

#########Interesse_Math  &  Mathe_LK###############
#####Test zwischen Interesse_Math  &  Mathe_LK
C(Interesse_Math,Mathe_LK)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 22.778, df = 6, p-value = 0.0008744
#Da p-value kleiner als 0.05 ist,
#gibt es einen signifikanten Zusammenhang zwischen Interesse_Math und Mathe_LK, 
#d.h. die Interesse_Math ist nicht unabhaengig vom Mathe_LK.
Mosaicplot_Math
#####Mosaicplot Erklaerung zwischen Interesse_Math  &  Mathe_LK
#Wenn wir uns die Mosaicplot_Math ansehen, können wir feststellen, 
#dass die meisten Menschen, die Math_LK in der Schule besucht haben, sich auf diesen Teil des hohen Interesses an Mathematik konzentrieren


#########Studienfach  &  Interesse_Math###############
#Test zwischen Studienfach & Interesse_Math
C(Studienfach,Interesse_Math)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 31.795, df = 18, p-value = 0.02325
#Da p-value kleiner als 0.05 ist,
#gibt es einen signifikanten Zusammenhang zwischen Studienfach
#und Interesse_Math, d.h. die Studienfach ist nicht unabhaengig vom Interesse_Math.
Mosaicplot_Math
#####Mosaicplot Erklaerung zwischen Studienfach & Interesse_Math
#Wenn wir uns das Mosaikdiagramm ansehen, können wir sehen, dass Mathematik-Studenten den höchsten Anteil an Personen mit einem hohen Interesse an Mathe haben. 
#An zweiter Stelle steht der Studiengang Informatik, gefolgt vom Studiengang Data Science. Das Studienfach Statistik ist am niedrigsten.





#####Mosaicplot zwischen Studienfach & Interesse_Prog & Mathe_LK
Mosaicplot_Prog<-mosaic(~Studienfach+Skala_p+Mathe_LK,data=Daten,highlighting = 'Mathe_LK', highlighting_fill=c('#FAFAD2','#DCDCDC'))

#########Studienfach  &  Interesse_Prog###############
#####Test zwischen Studienfach  &  Interesse_Prog
C(Studienfach,Interesse_Prog)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 58.401, df = 18, p-value = 3.699e-06
#Da p-value kleiner als 0.05 ist,
#gibt es einen signifikanten Zusammenhang zwischen Studienfach
#und Interesse_Math, d.h. die Studienfach ist nicht unabhaengig vom Interesse_Math.
Mosaicplot_Prog
#####Mosaicplot Erklaerung zwischen Studienfach  &  Interesse_Prog
#Wenn wir uns das Mosaicplot_Prog ansehen, können wir sehen, dass Informatik-Studenten den höchsten Prozentsatz an Menschen mit einem hohen Interesse an Programmierung haben. 
#An zweiter Stelle steht der Studiengang Data Science, gefolgt vom Studiengang Statistik. Das Studienfach Mathematik ist am niedrigsten.



#########Interesse_Prog  &  Mathe_LK###############
#####Test zwischen Interesse_Prog  &  Mathe_LK
C(Interesse_Prog,Mathe_LK)

#Pearson's Chi-squared test

#data:  x and y
#X-squared = 19.227, df = 6, p-value = 0.003797
#Da p-value kleiner als 0.05 ist,
#gibt es einen signifikanten Zusammenhang zwischen Interesse_Prog und Mathe_LK, 
#d.h. die Interesse_Prog ist nicht unabhaengig vom Mathe_LK.
Mosaicplot_Prog
#####Mosaicplot Erklaerung zwischen Interesse_Prog  &  Mathe_LK
#Wenn wir uns das Mosaikdiagramm ansehen, können wir sehen, 
#dass die überwiegende Mehrheit der Menschen mit einem hohen Interesse am Programmieren noch nie Math_LK in der Schule genommen hat.


#########Alter  &  Mathe_LK###############
D <- function(x, y){
  cor.test(x, y, method=c("pearson"))
}
#####Test zwischen  Alter  &  Mathe_LK
D(Alter,Mathe_LK)
#Pearson's product-moment correlation

#data:  x and y
#t = 1.0298, df = 98, p-value = 0.3056
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.0948813  0.2939102
#sample estimates:
    # cor 
#0.103465 

# Da p-value grosse als 0.05 ist,
#gibt es keinen signifikanten Zusammenhang zwischen Alter und Mathe_LK, 
#Der Korrelationskoeffizient zwischen ihnen ist sehr gering. 
#Der Korrelationskoeffizient betraegt 0,103465.



