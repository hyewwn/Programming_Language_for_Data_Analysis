#number1
multiply <- function(v) {
    for (i in (1:9)) {
      n <- v * i
      c <- cat(v, "*", i, "=", n, "\n")
    }
    return(c)
  }
  

multiply_table <- function() {
    for (k in (1:9)) {
      cat("\n","This is the table", k, "\n")
      multiply(k)
    }
  }
  
multiply_table()
  
  
#number2
i <- 0
m <- 0
for(k in (1:100)) {
    if(i<250) {
      m <- k
      i <- i + m 
    }
}
cat("합은", i, "이고, 마지막으로 더해진 수는", m, "이다.")
  
#number3
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
  
#number4
  m <- 0
  for(i in (1:100)) {
    if(i%%6 == 0) {
      m <- m+1
    }
  }
  cat("1부터 100까지 6의 배수는", m, "개이다.")
  
#number5
    x <- c(1:5)
  mat <- matrix(0, nrow=5, ncol=5)
  mat
  for(i in x) {
    for(j in x) {
      mat[i,j] <- abs(i-j)
    }
  }
  mat
  
#number6
    x <- c(1:5)
  for (i in x) {
    a <- rep('"*"', i)
    cat(a, "\n")
  }
  
  
#number7
    v <- c(1,2,3,4)
  for (i in (1:4)) {
    v[i] <- i^3
  }
  v
  
#number8
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
  
#number9
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
  
  
#number10
    sentence <- "데/이/터/사/이/언/스"
  wrong_sentence <- "테/이/어/사/이/언/스"
  sentence_letter <- strsplit(sentence,"/")
  wrong_sentence_letter <- strsplit(wrong_sentence, "/")
  m <- 0
  v <- vector()
  a <- unlist(sentence_letter)
  b <- unlist(wrong_sentence_letter)
  
  repeat {
    m <- m+1
    v <- c(v, subset(m, a[m] != b[m]))
    if(m>=length(a)) break
  }
  
  cat(v, "번째 글자가 틀렸습니다. '데이터사이언스'로 다시 입력하세요.")
  
#number11
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
max(v)
  
#number12
  i <- 0
  while(i<=10) {
    cat(i, "! =", factorial(i), "\n")
    i <- i+1
  }
  
#number13
  signal <- c('초록', '초록', '노랑', '빨강', '노랑', '초록')
  for(sig in signal)
    if(sig == '초록') {
      print(paste0(sig, '불입니다. 이동'))
    } else if(sig == '노랑'){
      print(paste0(sig, '불입니다. 천천히'))
    } else{
      print(paste0(sig, '불입니다. 정지'))
    }
  
#number14
    menus <- c('떡', '어묵', '소스', '떡볶이')
  calories <- c(541, 213, 120, NA)
  menu_cal <- data.frame(menus, calories)
  menu_cal
  
  #1
  cal_chk <- c('떡', '어묵', '소스')
  total_cal <- 0
  
  for(menu in cal_chk) {
    for(i in 1:nrow(menu_cal)) {
      if(menu_cal[i,1]==menu){
        cal=menu_cal[i,2]
        total_cal=total_cal + cal
        print(paste0(menu, '의 칼로리는 ',cal))
      }
    }
  }
  print(paste0('떡/어묵/소스 칼로리의 합은 ',total_cal))
  #2
  for(i in 1:nrow(menu_cal)) {
    for(j in 1:ncol(menu_cal)) {
      if(is.na(menu_cal[i,j])) {
        menu_cal[i,j]=total_cal
      }
    }
  }
  menu_cal
  
#number15
    student <- c('Annie', 'Theo', 'Steve', 'Hannah')
  grade1 <- c(85, 65, 85, 100)
  grade2 <- c(90, 75, 90, 90)
  grade3 <- c(75, 55, 80, 85)
  grade4 <- c(95, 75, 100, 90)
  
  math_grade <- data.frame(name=student, exam1=grade1, exam2=grade2, exam3=grade3, exam4=grade4)
  math_grade
  
  #1
  stu_mean <- function(x) {
    a <- (math_grade[x,2]+math_grade[x,3]+math_grade[x,4]+math_grade[x,5])/4
    return(a)
  }
  for(i in (1:4)) {
    if(stu_mean(i)>90) {
      print(paste0("이번 학기 ",math_grade[i,1],"의 평균 점수는 ", stu_mean(i), "점입니다."))
    }
  }
  
  #2
  exam_mean <- function(y) {
    y <- y+1
    b <- (math_grade[1,y]+math_grade[2,y]+math_grade[3,y]+math_grade[4,y])/4
    return(b)
  }
  
  for(j in (1:4)) {
    if(exam_mean(j)<80) {
      print(paste0(j,"번째 시험은 어려웠습니다."))
    }
  }
  
  #3
  compare <- function(z) {
    if(math_grade[z,2]>math_grade[z,3]) {
      p <- math_grade[z,2]
    }else {
      p <- math_grade[z,3]
    }
    
    if(math_grade[z,4]>math_grade[z,5]) {
      q <- math_grade[z,4]
    }else {
      q <- math_grade[z,5]
    }
    
    if(p>q) {
      r <- p
    }else {
      r <- q
    }
    return(r)
  }
  
  for(k in (1:4)) {
    if(compare(k)>90) {
      print(paste0("이번 학기 ", math_grade[k,1],"의 최고 점수는 ", compare(k), "점입니다."))
    }
  }
  
  
#number16
  install.packages('MASS')
  library(MASS)
  df = Cars93
  head(df)
  
  #1
  a <- levels(df[,1])
  for(i in (1:length(a))) {
    group <- subset(df, Manufacturer==a[i])
    mean <- mean(group$Price)
    print(paste0(a[i],"사 자동차의 평균 가격은 ", mean, "원이다."))
  }
  
  #2
  j <-0
  for(i in (1:length(df[,1]))) {
    if(sum(is.na(df[i,]>=2))) {
      df <- df[-i,]
      j <- j+1
    }
  }

  