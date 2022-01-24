## Teil 1 
# Yi

set.seed(1)
# (1)Alter:
Alter <- rnorm(100, mean = 25, sd = 2)
Alter <- round(Alter,0)
# (2)Studienfach:
fach <- c("Statistik", "Data Science", "Mathe", "Informatik") 

Studienfach <- sample(fach, 100, replace=TRUE, prob=c(0.3, 0.3, 0.25, 0.15))

# (3)Interesse an Mathematik:
interesse1 <- factor(1:7, levels = 1:7, ordered = TRUE)
Interesse_Math <- sample(interesse1, 100, replace=TRUE)



# Zusammenhang wird nicht berücksichtigt. Du kannst mein Code kucken.



# (4)Interesse an Programmieren:
interesse2 <- factor(1:7, levels = 1:7, ordered = TRUE)
Interesse_Prog <- sample(interesse2, 100, replace=TRUE)

df <- data.frame(Alter, Studienfach, Interesse_Math, Interesse_Prog)
# (5)Mathe-LK (ja/nein):
df$Mathe_LK <- ifelse( abs(as.numeric(df$Interesse_Math) - as.numeric(df$Interesse_Prog)) < "4", 1, 0)

df
setwd("/Users/is/Documents/GitHub/wissArbeiten_2122_Gruppe_11")
write.csv(x = df,file = "Daten.csv")

