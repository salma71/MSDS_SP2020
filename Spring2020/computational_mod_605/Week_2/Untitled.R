a <- matrix(c(2,4,-2,1,-1,5,3,3,5), ncol = 3)
a
get_U <- function(a) {
  U = a
  L = diag(nrow(a))
  n = nrow(a)
  for (i in 1:n) {
    k = seq(2, n)
    for (j in k) {
      if(j > i) {
        # get the multiplier and add it to the L matrix
        s = U[[j,i]]/U[[i,i]]
        L[j,i] = s
        # reduce by reduction and shuffle to the U matrix
        U[j,] = U[j,] - s * U[i,]
      }
    }
  }
  b = print(L,U)
  return(b)
}

value <- get_U(a)  
value