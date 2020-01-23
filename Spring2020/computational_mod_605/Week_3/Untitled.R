a <- matrix(c(1,2,3,4,
              -1,0,1,3), 2, byrow=T)
# a <- matrix(c(0,1,2,1,2,7,2,1,8), ncol = 3)
a

# get_rowinterchange <- function(a) {
#   U = a
#   n = ncol(a)
#   m = nrow(a)
#   counter = 0 # keeps track of how many times we swap => should be only one
#   # iterate each col of the matrix and search for the pivot element
#     for (i in 1:n) {
#       if(a[i,i] == 0 && counter < 1) {
#       # iterate each element in the columns
#         k = seq(2, m)
#         for (j in k) {
#             # get the pivot element
#             pivot = a[j,i]
#             pivot_inverse  = 1 / pivot
#             # swap the rows and multiple the swaped row with the pivot inverse
#             U[j,] = pivot_inverse * a[i,]
#             U[i,] = a[j,]
#             counter = counter + 1
#             break
#         } # end for - k
#       } # end if
#     } # end for i
#   return(U)
# }

# b =   get_rowinterchange(a)

get_U <- function(a) {
  U = a
  n = ncol(a)
  m = nrow(a)
  if(m == n) {
    for (i in 1:n) {
      for (j in 2:m) {
        if(U[j,i] != 0 & j > i) {
          # Add multiples of the pivot row to each of the lower rows, 
          # so every element in the pivot column of the lower rows equals 0.
          mplier = U[[j,i]]/U[[i,i]]
          # reduce by reduction and subtitute in the U matrix
          U[j,] = U[j,] - mplier * U[i,]
        } else if (U[j,i] != 0 & j == i) {
          U[j,] = U[j,] / U[[j,i]]
        }# end if
      } # end if 
    } # end for
  } else if(m < n) {
    for (i in 1:n) {
      for (j in 2:m) {
        if(U[j,i] != 0 & j > i) {
          U[i,] = U[i,] / U[[i,i]]
          # Add multiples of the pivot row to each of the lower rows, 
          # so every element in the pivot column of the lower rows equals 0.
          mplier = U[[j,i]]/U[[i,i]]
          # reduce by reduction and subtitute in the U matrix
          U[j,] = U[j,] - mplier * U[i,]
          
        } else if(U[j,i] != 0 & j == i) {
          U[i,] = U[i,] / U[[i,i]]
        } # end if
      } # end for
    } # end for
  } else if (m > n) {
    for (i in 1:n) {
      for (j in 2:m) {
        if(U[j,i] != 0 & j > i) {
          U[i,] = U[i,] / U[[i,i]]
          # Add multiples of the pivot row to each of the lower rows, 
          # so every element in the pivot column of the lower rows equals 0.
          mplier = U[[j,i]]/U[[i,i]]
          # reduce by reduction and subtitute in the U matrix
          U[j,] = U[j,] - mplier * U[i,]
          
        } else if(U[j,i] != 0 & j == i) {
          U[i,] = U[i,] / U[[i,i]]
        } # end if
      } # end for
    } # end for
  } # end if 
  return(round(U))
}

c = get_U(a)
c

ranking = function(cd) {
  rank = 0
  # sol = as.array(colSums(cd))
  # [1]  1  3  6 10
  for (i in 1:nrow(cd)) {
    if(sum(cd[i,]) > 0 & ncol(cd) == nrow(cd)) {
      rank = rank + 1
    } else if (sum(cd[i,]) > 0 & ncol(cd) > nrow(cd)) {
      rank = rank + 1
      # rank = max(nrow(cd), rank)
    } else if (sum(cd[i,]) > 0 & ncol(cd) < nrow(cd)){
      rank = rank + 1
      # rank = max(ncol(cd), rank)    
    }
  }
  return(rank)
}

d = ranking(c)
d