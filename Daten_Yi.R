## Teil 1 
# Yi

set.seed(1)
# (1)Alter:
Alter <- rnorm(100, mean = 25, sd = 2)
Alter <- round(Alter,0)

# (2)Studienfach:
fach <- c("Statistik", "Data Science", "Mathe", "Informatik") 
Studienfach <- sample(fach, 100, replace=TRUE, prob=c(0.3, 0.3, 0.25, 0.15))

df <- data.frame(Alter, Studienfach)
# (3)Interesse an Mathematik:
interesse1 <- factor(1:7, levels = 1:7, ordered = TRUE)
# Mathematik 
# Die meisten Menschen, die Mathematik studieren, interessieren sich dafür, daher liegt der Wert bei 5,6,7; 
# Aber es ist auch möglich, dass die Studenten, die Mathematik studieren, nicht auf Mathematik interessieren.
# Ich glaube die Wahrscheinlichkeit von der Wert 1:4 klein ist.
df$Interesse_Math <- sample(interesse1, 100 , prob = c(0.1, 0.2, 0.2, 0.3, 0.6, 0.7, 0.8), replace=TRUE)

# (4)Interesse an Programmieren:
# Analog zu Mathe.
interesse2 <- factor(1:7, levels = 1:7, ordered = TRUE)
df$Interesse_Prog <- sample(interesse2, 100, prob = c(0.1, 0.2, 0.2, 0.3, 0.6, 0.7, 0.8), replace=TRUE)

# (5)Mathe-LK (ja/nein):
# Annahme: 
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
setwd("/Users/is/Documents/GitHub/wissArbeiten_2122_Gruppe_11")
write.csv(x = df,file = "Daten_Yi.csv")

