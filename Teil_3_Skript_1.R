
###################################
###################################
# str()
#a)
###Interne Funktionen
mod <- function(x) {
     uniqv <- unique(x)
     uniqv[which.max(tabulate(match(x, uniqv)))]
   }

A <- function(x){
  print(mod(x))
  print(summary(x))
}

# b
B <- function(x){
  a <- levels(x)
  sum <- table(x)
  h <- prop.table(table(x))
  return(c(Art=a,Anzahl=sum,häufig=h))
}
#c)
library(DescTools)
C <- function(x,y){
  tab <- table(x,y)
  Assocs(tab)
  chisq.test(x,y)
}

#d)
D <- function(x, y){
  cor.test(x, y, method=c("pearson"))
}

#e)
###Interne Funktionen
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


#f)
