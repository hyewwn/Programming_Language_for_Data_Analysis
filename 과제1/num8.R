vec <- c(0,1,0,1,0,0,0,0,0,1,1,1)
a <- length(vec)
m <- 0
n <- 0
for(i in (1:a)) {
  if(vec[i]==0) {
    m <- m+1
  } else{
    n <- n+1
  }
}
cat("0의 개수는", m, ", 1의 개수는", n)
