x <- c(1:5)
for (i in x) {
  a <- rep('"*"', i)
  cat(a, "\n")
}
