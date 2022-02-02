#Schritt

f1<- function(x, na.omit=FALSE){ 
  if (na.omit) 
    x <- x[!is.na(x)] 
  m <- mean(x) 
  n <- length(x) 
  s <- sd(x) 
  var <- var(x)
  min <- min(x)
  max <- max(x)
  q<- quantile(x)
  Tukey <- fivenum(x)
  md <- median(x)
  skew <- sum((x-m)^3/s^3)/n 
  kurt <- sum((x-m)^4/s^4)/n - 3 
  mod <- function(x) {
    uniqv <- unique(x)
    uniqv[which.max(tabulate(match(x, uniqv)))]
  }
  
  return(c(n=n, mean=m,var=var,s=s, skew=skew, kurtosis=kurt,min=min,max=max,quantile=q,median=md,modul=mod(x))) } 
f1(mtcars$cyl)

f2 <- function(x){
  a <- levels(x)
  sum <- table(x)
  h <- prop.table(table(x))
  return(c(Art=a,Anzahl=sum,häufig=h))
}

f3 <- function(x,y){
  k <- table(x,y)
  chi <- chisq.test(k)$statistic
  
}
#x sind metrischen Variablen  y ist dichotomen Variablen
#Biserial
f4 <- function(x,y){
  y[y=="ja"] <- 1
  y[y=="nein"] <- -1
  cor.test(x, y)
}

f5 <- function(x){
  Q <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
  # Klasseneinteilung
  k<- numeric(length(x))
  k[x <= Q[2]] <- "niedrig"
  k[Q[1] <= x & x < Q[2]] <- "mittel"
  k[x >= Q[2]] <- "hoch"
  k[is.na(x)] <- NA
  Tafel <- table(k, x)
  Tafel}
