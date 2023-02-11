i <- 1
k <- 1
v <- vector()
repeat{
  v <- c(v,k)
  v <- c(v,i)
  k <- i + k
  i <- i + k
  if (k >= 377) break
}
print(v)
