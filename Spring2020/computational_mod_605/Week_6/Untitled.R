draw_pascal_2 <- function(n) {
  s <- matrix(vector('numeric'))
  for(i in 0:(n-1)) {
    # for(k in 0:(n-i))
    #   s <- paste(s, "  ", sep="")
    for(j in 0:i) {
      s[j,] <- append(s, choose(i, j))
    }
  }
  return(s)
  
}

pp = draw_pascal_2(10)
pp