#a)

mod <- function(x) {
uniqv <- unique(x)
uniqv[which.max(tabulate(match(x, uniqv)))]
}


#e)
intern <- function(x){
  min(x):max(x)
}
  