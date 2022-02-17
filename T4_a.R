##Teil4
#a)
Daten <- read_csv("Daten.csv")
View(Daten)
mod <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

A <- function(x){
  print(mod(x))
  print(summary(x))
}
A(Daten$Alter)

