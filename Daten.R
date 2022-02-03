### Aufgabenstellung 3.Teilleistung Gruppenarbeit/GitHub
### Teil 1 
### Haiyi Rong & Yi Sun

# Die beiden R-Codes sind in diesem Dokument zusammengefasst.


set.seed(1)

## (1) Alter:
Alter <- rnorm(100, mean = 25, sd = 2)
Alter <- round(Alter,0)



## (2) Studienfach:
fach <- c("Statistik", "Data Science", "Mathe", "Informatik") 
Studienfach <- sample(fach, 100, replace=TRUE, prob=c(0.35, 0.35, 0.2, 0.1))

df <- data.frame(Alter, Studienfach)



## (3) Interesse an Mathematik:
IM <- factor(1:7, levels = 1:7, ordered = TRUE)
df$Interesse_Math <- sample(IM, 100, replace = TRUE)

# Annahme：
# *Ausnahmen können existieren.
# a) Studienfach Mathe:
# Die meisten Menschen, die Mathe studieren, haben ein großes Interesse an Mathematik.
# Je geringer das Interesse an Mathematik, desto geringer ist die Wahrscheinlichkeit, Mathe zu studieren.
# Also setzen wir die Wahrscheinlichkeit für der Wert 1:7 als 0.05, 0.1, 0.2, 0.3, 0.4, 0.8 und 0.9.
df$Interesse_Math[df$Studienfach == c("Mathe")] <- sample(IM, length(df$Interesse_Math[df$Studienfach == c("Mathe")]), prob = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.8, 0.9), replace=TRUE)

# b) Studienfach Statistik:
# Die meisten Menschen, die Statistik studieren,haben ein relativ großes Interesse an Mathematik.
# Also setzen wir die Wahrscheinlichkeit für der Wert 1:7 als 0.05, 0.1, 0.2, 0.9, 0.8, 0.5 und 0.4. 
df$Interesse_Math[df$Studienfach == c("Statistik")] <- sample(IM, length(df$Interesse_Math[df$Studienfach == c("Statistik")]), prob = c(0.05, 0.1, 0.2, 0.9, 0.8, 0.5, 0.4), replace=TRUE)

# c) Studienfach Data Science:
# Die meisten Menschen, die Data Science studieren, haben ein relativ großes Interesse an Mathematik.
# Also setzen wir die Wahrscheinlichkeit für der Wert 1:7 als 0.05, 0.1, 0.2, 0.9, 0.8, 0.5 und 0.4.
df$Interesse_Math[df$Studienfach == c("Data Science")] <- sample(IM, length(df$Interesse_Math[df$Studienfach == c("Data Science")]), prob = c(0.05, 0.1, 0.2, 0.9, 0.8, 0.5, 0.4), replace=TRUE)

# d) Studienfach Informatik:
# Die meisten Menschen, die Informatik studieren, haben nur wenig Interesse am Mathematik.
# Je geringer das Interesse an Mathematik ist, desto höher ist die Wahrscheinlichkeit, Informationen zu studieren.
# Also setzen wir die Wahrscheinlichkeit für der Wert 1:7 als 0.9, 0.8, 0.4, 0.3, 0.2, 0.1 und 0.05.
df$Interesse_Math[df$Studienfach == c("Informatik")] <- sample(IM, length(df$Interesse_Math[df$Studienfach == c("Informatik")]), prob = c(0.9, 0.8, 0.4, 0.3, 0.2, 0.1, 0.05), replace=TRUE)
df$Interesse_Math <- factor(df$Interesse_Prog, levels = 1:7, order = TRUE)


## (4) Interesse an Programmieren:

IP <- factor(rep(1:7,1), levels = 1:7, order = TRUE)
df$Interesse_Prog <- sample(IP, 100, replace = TRUE)

# Annahme：
# *Ausnahmen können existieren.

# a) Studienfach Mathe:
# Die meisten Menschen, die Mathe studieren, haben nur wenig Interesse am Programmierung.
# Je geringer das Interesse an Programmierung ist, desto höher ist die Wahrscheinlichkeit, Informatik zu studieren.
# Also setzen wir die Wahrscheinlichkeit für der Wert 1:7 als 0.9, 0.8, 0.4, 0.3, 0.2, 0.1 und 0.05.
df$Interesse_Prog[df$Studienfach == c("Mathe")] <- sample(IM, length(df$Interesse_Prog[df$Studienfach == c("Mathe")]), prob = c(0.9, 0.8, 0.4, 0.3, 0.2, 0.1, 0.05), replace=TRUE)

# b) Studienfach Statistik:
# Die meisten Menschen, die Statistik studieren,haben ein relativ großes Interesse an Programmierung.
# Also setzen wir die Wahrscheinlichkeit für der Wert 1:7 als 0.05, 0.1, 0.2, 0.9, 0.8, 0.5 und 0.4. 
df$Interesse_Prog[df$Studienfach == c("Statistik")] <- sample(IM, length(df$Interesse_Prog[df$Studienfach == c("Statistik")]), prob = c(0.05, 0.1, 0.2, 0.9, 0.8, 0.5, 0.4), replace=TRUE)

# c) Studienfach Data Science:
# Die meisten Menschen, die Data Science studieren, haben ein relativ großes Interesse an Programmierung. (Mehr als Statistik)
# Also setzen wir die Wahrscheinlichkeit für der Wert 1:7 als 0.05, 0.1, 0.2, 0.9, 0.8, 0.6 und 0.5.
df$Interesse_Prog[df$Studienfach == c("Data Science")] <- sample(IM, length(df$Interesse_Prog[df$Studienfach == c("Data Science")]), prob = c(0.05, 0.1, 0.2, 0.9, 0.8, 0.6, 0.5), replace=TRUE)

# d) Studienfach Informatik:
# Die meisten Menschen, die Informatik studieren, haben ein großes Interesse an Programmierung.
# Je geringer das Interesse an Programmierung, desto geringer ist die Wahrscheinlichkeit, Informatik zu studieren.
# Also setzen wir die Wahrscheinlichkeit für der Wert 1:7 als 0.05, 0.1, 0.2, 0.3, 0.4, 0.8 und 0.9.
df$Interesse_Prog[df$Studienfach == c("Informatik")] <- sample(IM, length(df$Interesse_Prog[df$Studienfach == c("Informatik")]), prob = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.8, 0.9), replace=TRUE)
df$Interesse_Prog <- factor(df$Interesse_Prog, levels = 1:7, order = TRUE)


## (5) Mathe-LK (ja/nein):

# Annahme: 
# 1 <- Ja, 0 <- Nein 
# *Ausnahmen können existieren.
# Dummy-Variable hier erstellen.
# a) Die Studenten, die ein größeres Interesse an Mathematik haben und die Mathematik studienren,
#    die in der Schule Mathe-LK hatten.
#    D.h. Es ist sehr wahrscheinlich, dass die Werte ihres Interesses_Math 5, 6 und 7 sind.
df$Mathe_LK[df$Studienfach == c("Mathe")] <- ifelse( df$Interesse_Math[df$Studienfach == c("Mathe")]  > "4", 1, 0)
# b) Die meisten Studenten, die ein kleines Interesse an Mathematik haben und Informatik studieren, 
#    in der Schule Mathe-LK nicht hatten.
#    D.h. Es ist sehr wahrscheinlich, dass die Werte ihres Interesses_Mathe 1, 2 und 3 sind.
df$Mathe_LK[df$Studienfach == c("Informatik")] <- ifelse( df$Interesse_Math[df$Studienfach == c("Informatik")]  < "4", 1, 0)
# c) Die Studenten, die Studienfach Statistik oder Data Science studieren, mehres Interesse an Mathematik als Programierung,
#    die in der Schule Mathe_LK hatten.
#    D.h. Es ist sehr wahrscheinlich, dass die Differenz ziwischen ihres Interesses_Mathe und Interesses_Prog größer als 3 ist.
df$Mathe_LK[df$Studienfach == c("Statistik")] <- 
  ifelse((as.numeric(df$Interesse_Math[df$Studienfach == c("Statistik")]) - as.numeric(df$Interesse_Prog[df$Studienfach == c("Statistik")])) >= "3", 1, 0)
df$Mathe_LK[df$Studienfach == c("Data Science")] <- 
  ifelse((as.numeric(df$Interesse_Math[df$Studienfach == c("Data Science")]) - as.numeric(df$Interesse_Prog[df$Studienfach == c("Data Science")])) >= "3", 1, 0)


df
str(df)
setwd("/Users/is/Documents/GitHub/wissArbeiten_2122_Gruppe_11")
write.csv(x = df,file = "Daten.csv")

read.csv("/Users/is/Documents/GitHub/wissArbeiten_2122_Gruppe_11/Daten.csv")
