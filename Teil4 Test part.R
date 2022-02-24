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
#p-value ist kleiner als 0.05.
#es gibt einen signifikanten Zusammenhang zwischen Studienfach
#und Math_LK, d.h. die Studienfach ist nicht unabh??ngig vom Math_LK.
C(Studienfach,Interesse_Math)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 31.795, df = 18, p-value = 0.02325
#p-value ist kleiner als 0.05.
#es gibt einen signifikanten Zusammenhang zwischen Studienfach
#und Interesse_Math, d.h. die Studienfach ist nicht unabh??ngig vom Interesse_Math.
C(Studienfach,Interesse_Prog)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 58.401, df = 18, p-value = 3.699e-06
#p-value ist kleiner als 0.05.
#es gibt einen signifikanten Zusammenhang zwischen Studienfach
#und Interesse_Math, d.h. die Studienfach ist nicht unabh??ngig vom Interesse_Math.

C(Interesse_Math,Mathe_LK)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 22.778, df = 6, p-value = 0.0008744
#p-value ist kleiner als 0.05.
#es gibt einen signifikanten Zusammenhang zwischen Interesse_Math und Mathe_LK, 
#d.h. die Interesse_Math ist nicht unabh??ngig vom Mathe_LK.
C(Interesse_Prog,Mathe_LK)
#Pearson's Chi-squared test

#data:  x and y
#X-squared = 19.227, df = 6, p-value = 0.003797
#es gibt einen signifikanten Zusammenhang zwischen Interesse_Math und Mathe_LK, 
#d.h. die Interesse_Math ist nicht unabh??ngig vom Mathe_LK.