calculate_gauss <- function(a, b) {
  n <- nrow(a)
  m <- ncol(a)
  # det <- 1
  # computes the array of col maximal elements
  # loop over columns to get the max element in col
  for (i in 1:n) {
    j <- which.max(a[i:n, i]) + i - 1
    if (a[i, i] == 0) {
      a[c(i, j), i:n] <- a[c(j, i), i:n]
      b[c(i, j), ] <- b[c(j, i), ]
    }
  
    # finds the multiplier to eliminate
    # if(a[j,i] != 0) {
      # k <- seq(i + 1, n)
      for (k in 2:m) {
          if(a[k,i] != 0 & k > i) {
            s <- a[k, i] / a[i, i] 
            a[k, ] <- a[k, ] - s * a[i, ]
            # do the same for the other vector
            b[k, ] <- b[k, ] - s * b[i, ]
          }
        }
    # }
  }
  
  # do the same for rows
  for (i in seq(n, 1)) {
    if (i < n) {
      for (j in seq(i + 1, n)) {
        b[i, ] <- b[i, ] - a[[i, j]] * b[j, ]
      }
    }
    b[i, ] <- b[i, ] / a[[i, i]]
  }
  
  return(x=b)
}
a <- matrix(c(0,2,-1,1,-1,-2,3,5,4), nrow = 3, ncol = 3)
b <- matrix(c(1,2,6), nrow = 3, ncol = 1)

val <- calculate_gauss(a,b)
val
