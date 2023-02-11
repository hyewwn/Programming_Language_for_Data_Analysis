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
