#v*1 ~ v*9까지 계산하는 함수
multiply <- function(v) {
  for (i in (1:9)) {
    n <- v * i
    c <- cat(v, "*", i, "=", n, "\n")
  }
  return(c)
}

#구구단 출력함수 생성
multiply_table <- function() {
  for (k in (1:9)) {
  cat("\n","This is the table", k, "\n")
  multiply(k)
  }
}

#구구단 출력
multiply_table()

