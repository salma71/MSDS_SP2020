a <- matrix(c(2,4,-2,
              1,-1,5,
              3,3,5), nrow = 3, byrow = TRUE)
a

myinverse <- function(A){
  # Get the rank of the input matrix
  rank_a <- qr(A)$rank
  # create a diagonal matrix with a size of the rank
  d <- diag(rank_a)
  # loop over each row and column and get the determinent
  for (i in 1:nrow(A)) { 
    for (j in 1:ncol(A)){ 
      d[i,j]=((-1)^(i+j))*det(A[-i,-j])
    }
  }
  return(t(d)/det(A))
}

vlaue <- myinverse(a)