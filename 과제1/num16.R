install.packages('MASS')
library(MASS)
df = Cars93
head(df)

#1
a <- levels(df[,1])
length(a)
for(i in (1:length(a))) {
  group <- subset(df, Manufacturer==a[i])
  mean <- mean(group$Price)
  print(paste0(a[i],"사 자동차의 평균 가격은 ", mean, "원이다."))
}

#2
j <-0
for(i in (1:length(df[,1]))) {
  while() {
    df <- df[-i,]
    j <- j+1
  }
}
j
