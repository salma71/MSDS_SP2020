draw_pascal <- function(n) {
  pascal_tri = matrix()
  for (i in 0:n) {
    pascal_tri[,i] = vector(get_coeff(n, i) , mode = 'numeric', length = i)
  }
  return(pascal_tri)
}



# C(n, k) = n! / (n-k)! * k!
#   = [n * (n-1) *....* 1]  / [ ( (n-k) * (n-k-1) * .... * 1) * 
#                                 ( k * (k-1) * .... * 1 ) ]
# After simplifying, we get
# C(n, k) = [n * (n-1) * .... * (n-k+1)] / [k * (k-1) * .... * 1]
# 
# Also, C(n, k) = C(n, n-k)  // we can change r to n-r if r > n-r 

get_coeff <- function(n, k) {
  if(k > n - k){
    k = n - k
  }
  res <- 1
  # Calculate value of  
  # [n * (n-1) *---* (n-k + 1)] / [k * (k-1) *----* 1] 
    for (i in 0:(k-1)) {
      res <- res * (n - i)
      res <- res / (i + 1)
    }
  return (res)
  
  }

value = get_coeff(8, 2)

pas = draw_pascal(4)



