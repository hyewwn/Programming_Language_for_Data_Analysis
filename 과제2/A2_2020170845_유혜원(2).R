#1.완전탐색
output <- function(x) {
  put <- sample(1:50,20)
  k <- vector()
  for(i in 1:length(put)) {
    k <- c(k, x==put[i])
  }
  return(T %in% k)
}

#2.거스름돈
exchange <- function(x) {
  i <- 0
  while(x>=500) {
   x <- x-500
   i <- i+1
  }
  while(x>=100) {
   x <- x-100
   i <- i+1
  }
  while(x>=10) {
   x <- x-10
   i <- i+1
  }
  return(i)
}

#3.영화 평점 분석
movie <- function(x) {
  mean <- mean(x)
  pre_mean <- x[length(x)]-mean
  if(pre_mean<0) {
    return(0)
  } else if (pre_mean>10) {
    return(10)
  } else {
    return(pre_mean)
  }
}
a <- sample(0:10,5)
movie(a)

#4.불량품 검출
produce <-function(x) {
  y <- vector()
  for(i in (1:length(x))) {
    y <- c(y,x[i]*(sample(1:5,1)))
  }
  return(y)
}

test <- function(x) {
  y <- produce(x)
  z <- y[y <= 30 & y>= 10]
  return(length(z))
}

a <- sample(1:30, 10)
test(a)

#5.계산기
calculator <- function(a,b,c) {
  if(c=='+') {
   sprintf("%d%s%d의 결과는 %d 입니다", a,c,b,a+b)
  } else if(c=='-') {
    sprintf("%d%s%d의 결과는 %d 입니다", a,c,b,a-b)
  } else if(c=='*') {
    sprintf("%d%s%d의 결과는 %d 입니다", a,c,b,a*b)
  } else if(c=='/') {
    print(paste0(a,'/',b,'의 몫은 ', a%/%b,'이며, 나머지는 ', a%%b,'입니다.'))
  }
}

#6.피보나치 수
fibo <- function(x) {
  if(x>2) {
    a <- fibo(x-2)+fibo(x-1)
    return(a)
  } else if(x==1|x==2){
    return(1)
  }
}

#7.*로 이루어진 삼각형
install.packages("stringr")
library(stringr)

triangle_while <- function(x) {
  k<-0
  while(k < x) {
    k <- k+1
    print(str_dup("*",times=k))
  }
}

triangle_for <- function(x) {
  for(i in 1:x) {
    print(str_dup("*",times=i))
  }
}

#8.AI와의 가위바위보 대결
rps <- c("rock","sisor","paper")

judge <- function(x,y,n) {
  a <- 0
  b <- 0
  c <- 0
  for(i in 1:n) {
    if(x[i]==y[i]){
      print("무승부")
      c <- c+1
    } else if(x[i]==rps[1]){
      if(y[i]==rps[2]) {
        print("패")
        b <- b+1
      }else if(y[i]==rps[3]){
        print("승")
        a <- a+1
      }
    } else if(x[i]==rps[2]){
      if(y[i]==rps[1]) {
        print("승")
        a <- a+1
      }else if(y[i]==rps[3]){
        print("패")
        b <- b+1
      }
    } else if(x[i]==rps[3]){
      if(y[i]==rps[1]) {
        print("패")
        b <- b+1
      }else if(y[i]==rps[2]){
        print("승")
        a <- a+1
      }
    }
  }
  return(cat("영탁군은 AI에 대항하여",a,"승,",b,"패,",c,"무승부를 기록하였습니다."))
}

AI_rps <- function(x) {
  AI <- sample(rps,x,replace=TRUE)
  YT <- sample(rps,x,replace=TRUE)
  judge(AI,YT,x)
}

#9.BubbleSort
bubblesort <- function(x){
  n<-length(x)
  for(i in 1:(n-1)){
    for(k in 1:(n-i)){
      if(x[k]>x[k+1]){
        a<-x[k]
        x[k]<-x[k+1]
        x[k+1]<-a
      }
    }
  }
  return(x)
}
put<-sample(1:1000,100)
put
bubblesort(put)

system.time(bubblesort(put))


#10.Quick sort
quicksort <- function(x) {
  if(length(x)>1) {
    n <- sample(1:length(x),1)
    pivot <- x[n]
    less <- x[x<pivot]
    more <- x[x>pivot]
    k <- c(quicksort(less), pivot, quicksort(more))
    return(k)
  }else{
    return(x)
  }
}

put <- sample(1:1000, 100)
put
quicksort(put)
system.time(quicksort(put))

