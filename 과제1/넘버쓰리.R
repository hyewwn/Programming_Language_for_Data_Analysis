top <- c("후드티", "체크셔츠", "줄무늬셔츠", "니트", "파자마상의")
bottom <- c("청바지", "슬랙스", "트레이닝바지", "면바지", "파자마하의")

cat("가능한 나의 선택지는 아래와 같습니다")

pickcloth <- function() {
  i <- 1
  k <- 1
  while(top[i]!=top[5]) {
    while(bottom[k] != bottom[5]) {
      cat(top[i],"와",bottom[k], "를 고르셨습니다.", "\n")
      k <- k+1
    }
    k <- 1
    i <- i+1
  }
  while(top[i]==top[5]) {
    while(bottom[k] != bottom[5]) {
      cat(top[i],"와",bottom[k], "를 고르셨습니다. 다시 골라주세요!", "\n")
      k <- k+1
    }
    i <- 1
  }
  while(bottom[k]==bottom[5]) {
    while(top[i] != top[5]) {
      cat(top[i],"와",bottom[k], "를 고르셨습니다. 다시 골라주세요!", "\n")
      i <- i+1
    }
    k <- 1
  }
  i <- 5
  k <- 5
  while(bottom[k]==bottom[5]) {
    while(top[i] == top[5]) {
      cat(top[i],"와",bottom[k], "를 고르셨습니다.", "\n")
      i <- 1
    }
    k <- 1
  }
}

pickcloth()
