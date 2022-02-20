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

######Alter plot############
library(hrbrthemes)
library(viridis)
Daten%>%
ggplot( aes(x = Studienfach, y = Alter,  fill = Studienfach)) +
  geom_boxplot() +
  xlab("")+
  scale_fill_viridis(discrete = TRUE,option = "E" ,alpha=0.6) +
  ggtitle("Boxplots des Alters nach Studienfach ")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11) )

######Anzahl Studienfach#########

ggplot(data = Daten,aes(x =Studienfach ,fill=Studienfach))+
  geom_bar(stat = "count",position = "dodge")+
  ylab("Absolute Haufigkeit")+
  xlab("")+
  scale_fill_viridis(discrete = T,alpha=0.6,option = "E") +
  ggtitle("Histogramm der Anzahl der Studienfach") +
  theme_ipsum()

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

ggplot(data = Daten,aes(x=Skala_p,fill=Studienfach))+
  geom_bar(stat = "count",position="dodge")+
  theme(axis.title.x=element_blank())+
  ylab("Anzahl")

ggplot(data = Daten,aes(x=Skala_m,fill=Studienfach))+
  geom_bar(stat = "count",position="dodge")+
  theme(axis.title.x=element_blank())+
  ylab("Anzahl")

