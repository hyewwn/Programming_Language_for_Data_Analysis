x <- c(1:5)
mat <- matrix(0, nrow=5, ncol=5)
mat
for(i in x) {
  for(j in x) {
    mat[i,j] <- abs(i-j)
  }
}
mat