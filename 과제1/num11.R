A <- list(100, -0.009, 3, -30, -0.10)
B <- list(-20, 2, -0.9, 0.085, 5)

v <- vector()
for(i in (1:length(A))) {
  for(j in (1:length(B))) {
    v <- c(v, A[[i]]+B[[j]])
    v <- c(v, A[[i]]-B[[j]])
    v <- c(v, A[[i]]*B[[j]])
    v <- c(v, A[[i]]/B[[j]])
    v <- c(v, B[[j]]/A[[i]])
  }
}
v
max(v)
