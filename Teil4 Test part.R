library(mvtnorm)
library(rstudioapi)
library(gld)
library(lmom)
library(e1071)
library(proxy)
library(expm)
library(Exact)
library(DescTools)
library(rootSolve)

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
C(Studienfach,Interesse_Math)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 31.795, df = 18, p-value = 0.02325
#Da p-value kleiner als 0.05 ist,
#gibt es einen signifikanten Zusammenhang zwischen Studienfach
#und Interesse_Math, d.h. die Studienfach ist nicht unabhaengig vom Interesse_Math.
C(Studienfach,Interesse_Prog)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 58.401, df = 18, p-value = 3.699e-06
#Da p-value kleiner als 0.05 ist,
#gibt es einen signifikanten Zusammenhang zwischen Studienfach
#und Interesse_Math, d.h. die Studienfach ist nicht unabhaengig vom Interesse_Math.

C(Interesse_Math,Mathe_LK)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 22.778, df = 6, p-value = 0.0008744
#Da p-value kleiner als 0.05 ist,
#gibt es einen signifikanten Zusammenhang zwischen Interesse_Math und Mathe_LK, 
#d.h. die Interesse_Math ist nicht unabh??ngig vom Mathe_LK.
C(Interesse_Prog,Mathe_LK)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 19.227, df = 6, p-value = 0.003797
#Da p-value kleiner als 0.05 ist,
#gibt es einen signifikanten Zusammenhang zwischen Interesse_Prog und Mathe_LK, 
#d.h. die Interesse_Prog ist nicht unabhaengig vom Mathe_LK.


D <- function(x, y){
  cor.test(x, y, method=c("pearson"))
}
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

