#install packages & set up
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("treemap")

options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(treemap)

#read data
households <- read.csv(file="Percentage of households owning a house.csv", header=TRUE)

#households_total-------------------------------------------------------------------
households_total <- households[households$주택자산.10분위별=="총계",]

str(households)

g1 <- ggplot(households_total, aes(x=연령대별 , y=X2015.년, group=1)) +
      geom_point() +
      scale_x_discrete(limit=c("30세미만",
                           "30~39세",
                           "40~49세",
                           "50~59세",
                           "60~69세",
                           "70~79세",
                           "80세이상")) +
      geom_line(linetype="dashed", col="blue", size=1) +
      labs(title="2015년 연령대별 주택소유 가구수 비율",y="주택 소유 가구수 비율",x="연령대별",caption="통계청")

g2 <- ggplot(households_total, aes(x=연령대별 , y=X2016.년, group=1)) +
  geom_point() +
  scale_x_discrete(limit=c("30세미만",
                           "30~39세",
                           "40~49세",
                           "50~59세",
                           "60~69세",
                           "70~79세",
                           "80세이상")) +
  geom_line(linetype="dashed", col="blue", size=1) +
  labs(title="2016년 연령대별 주택소유 가구수 비율",y="주택 소유 가구수 비율",x="연령대별",caption="통계청") 

g3 <- ggplot(households_total, aes(x=연령대별 , y=X2017.년, group=1)) +
  geom_point() +
  scale_x_discrete(limit=c("30세미만",
                           "30~39세",
                           "40~49세",
                           "50~59세",
                           "60~69세",
                           "70~79세",
                           "80세이상")) +
  geom_line(linetype="dashed", col="blue", size=1) +
  labs(title="2017년 연령대별 주택소유 가구수 비율",y="주택 소유 가구수 비율",x="연령대별",caption="통계청")

g4 <- ggplot(households_total, aes(x=연령대별 , y=X2018.년, group=1)) +
  geom_point() +
  scale_x_discrete(limit=c("30세미만",
                           "30~39세",
                           "40~49세",
                           "50~59세",
                           "60~69세",
                           "70~79세",
                           "80세이상")) +
  geom_line(linetype="dashed", col="blue", size=1) +
  labs(title="2018년 연령대별 주택소유 가구수 비율",y="주택 소유 가구수 비율",x="연령대별",caption="통계청")

g5 <- ggplot(households_total, aes(x=연령대별 , y=X2019.년, group=1)) +
  geom_point() +
  scale_x_discrete(limit=c("30세미만",
                           "30~39세",
                           "40~49세",
                           "50~59세",
                           "60~69세",
                           "70~79세",
                           "80세이상")) +
  geom_line(linetype="dashed", col="blue", size=1) +
  labs(title="2019년 연령대별 주택소유 가구수 비율",y="주택 소유 가구수 비율",x="연령대별",caption="통계청")

gridExtra::grid.arrange(g1,g2,g3,g4,g5, ncol=2, nrow=3)

#households_Nototal-------------------------------------------------------------------
households_Nototal <- households[households$주택자산.10분위별!="총계",]

ggplot(households_Nototal, aes(x=연령대별 , y=X2019.년, group=1)) +
  geom_point(aes(col=주택자산.10분위별)) +
  geom_line(aes(col =주택자산.10분위별, group=주택자산.10분위별)) + 
  scale_x_discrete(limit=c("30세미만",
                           "30~39세",
                           "40~49세",
                           "50~59세",
                           "60~69세",
                           "70~79세",
                           "80세이상")) +
  labs(title="2019년 연령대/분위별 주택소유 가구수 비율",y="주택 소유 가구수 비율",x="연령대별",caption="통계청") + 
  scale_colour_brewer(palette= "Spectral") +
  scale_y_continuous(breaks=seq(0,3,0.5),
    labels=sprintf("%1.2f%%",seq(0,3,0.5)))

#거주지역/성/연령대별 주택소유 가구수-------------------------------------------------------------------
#read data
households2 <- read.csv(file="거주지역 성 연령대별 주택소유 가구수.csv", header=TRUE)
households2 <- households2[households2$성별=="총계",]
households2 <- filter(households2,households2$연령대별!="총계")

households3_1 <- filter(households2, households2$거주지역별=="서울특별시")
for(i in 6:10) {
  households3_1[,i] <- as.integer(households3_1[,i])
}
households3_2 <- filter(households2, households2$거주지역별=="세종특별자치시")
for(i in 6:10) {
  households3_2[,i] <- as.integer(households3_2[,i])
}

gg1 <- ggplot(households3_1, aes(x=연령대별, y=X2019.년)) +
  geom_point(size=5) + 
  geom_segment(aes(xend=연령대별, yend=0)) +
  scale_x_discrete(limit=c("30세미만",
                           "30~39세",
                           "40~49세",
                           "50~59세",
                           "60~69세",
                           "70~79세",
                           "80세이상")) +
  labs(title="2019년 서울 연령별 주택소유 가구수",y="주택소유가구수",x="연령대별", caption="통계청") +
  theme_classic()
gg2 <- ggplot(households3_2, aes(x=연령대별, y=X2019.년)) +
  geom_point(size=5) + 
  geom_segment(aes(xend=연령대별, yend=0)) +
  scale_x_discrete(limit=c("30세미만",
                           "30~39세",
                           "40~49세",
                           "50~59세",
                           "60~69세",
                           "70~79세",
                           "80세이상")) +
  labs(title="2019년 세종 연령별 주택소유 가구수",y="주택소유가구수",x="연령대별", caption="통계청") +
  theme_classic()

gridExtra::grid.arrange(gg1,gg2, ncol=2, nrow=1)

#분위별 거주지역별 주택소유 가구수 비율-------------------------------------------------------------------
households4 <- read.csv(file="분위별 거주지역별 주택소유 가구수 비율.csv", header=TRUE)    
households4_Nototal <- filter(households4,households4$주택자산.10분위별!="총계")

a <- ggplot(households4_Nototal, aes(x=거주지역별 , y=주택자산.10분위별)) +
  geom_point(aes(col=거주지역별, size=X2019.년)) + 
  scale_y_discrete(limit=c("1분위(하위10%)",
                           "2분위",
                           "3분위",
                           "4분위",
                           "5분위",
                           "6분위",
                           "7분위",
                           "8분위",
                           "9분위",
                           "10분위(상위10%)")) +
  labs(title="2019년 거주지역/분위별 주택소유 가구수 비율",y="분위",x="거주지역", size="주택소유비율(%)",caption="통계청")

a + theme(axis.text.x=element_text(size=6, face="bold")) +
  guides(col=F)


#DMB재난경보발령현황-------------------------------------------------------------------
DMB <- read.csv(file="DMB재난경보방송발령현황(2019년).csv", header=TRUE)
unique <- unique(DMB$재난종류)
num <- vector()
for(i in 1:length(unique)){
  DMB_1 <- filter(DMB,DMB$재난종류==unique[i])
  num_1 <- nrow(DMB_1)
  num <- c(num, num_1)
}
df <- data.frame(unique,num)

#어떤 재난경보가 많이 울리는지
ggplot(df, aes(x="", y=num, fill=unique)) +
  geom_bar(stat="identity", width=1, col="black") +
  coord_polar(theta="y") +
  labs(fill="재난종류", 
       x=NULL, 
       y=NULL, 
       title="재난종류별 재난경보 발령 횟수", 
       caption="공공데이터포털") +
  theme(title = element_text(size=11, face="bold"))


#중국/일본 무역 비교
port <- read.csv(file="품목별 국가별 수출입실적.csv", header=T)
port[,9] <- as.integer(gsub(",","",port[,9]))

port_china <- filter(port, port$국가명=="중국")
port_japan <- filter(port, port$국가명=="일본")

port_china$흑적자 <- ifelse(port_china$무역수지 < 0, "below", "above")
port_japan$흑적자 <- ifelse(port_japan$무역수지 < 0, "below", "above")

ggg1 <- ggplot(port_china, aes(x=품목명, y=무역수지, label=무역수지)) + 
  geom_bar(stat='identity', aes(fill=흑적자), width=.5) +
  theme(axis.text.y=element_blank()) +
  coord_flip() +
  scale_fill_manual(name="흑적자", 
                    labels = c("흑자", "적자"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(title= "중국무역현황")

ggg2 <- ggplot(port_japan, aes(x=품목명, y=무역수지, label=무역수지)) + 
  geom_bar(stat='identity', aes(fill=흑적자), width=.5) +
  theme(axis.text.y=element_blank()) +
  coord_flip() +
  scale_fill_manual(name="흑적자", 
                    labels = c("흑자", "적자"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(title= "일본무역현황")

gridExtra::grid.arrange(ggg1,ggg2, ncol=1, nrow=2)                   


#근로복지공단 소속병원 연도별 건강진단 및 작업환경측정 현황
examination <- read.csv(file="근로복지공단 소속병원 연도별 건강진단 및 작업환경측정 현황.csv", header=T)
for(i in 2:4){
examination[,i] <- as.integer(gsub(",","",examination[,i]))
}
examination[,1] <- as.integer(examination[,1])

year <- c(examination$년도)
type <- rep(c("일반검진","특수검진","채용신검"),times=nrow(examination))
value <- vector()
for(i in 1:nrow(examination)) {
  value <- c(value, examination[i,2])
  value <- c(value, examination[i,3])
  value <- c(value, examination[i,4])
}
data <- data.frame(year,type,value)

ggplot(data, aes(x=year, y=value, fill=type)) + 
  geom_area(position = "fill") +
  scale_fill_brewer(palette="Blues") +
  theme_classic() +
  labs(title="건강진단 비율",caption="공공데이터포털",x="년도",y="건강진단비율",fill="진단종류")


#근로복지지원 현황
laborwelfare <- read.csv(file="근로복지지원현황.csv", header=T)

laborwelfare %>% group_by(기준년월) %>%
  summarise(최소=min(지원자수),최다=max(지원자수)) %>%
  ggplot(aes(x=기준년월, y=최다)) + 
  geom_point(size=3) +
  geom_point(aes(y=최소), size=3) +
  geom_segment(aes(xend=기준년월, yend=최소)) +
  scale_x_continuous(breaks=seq(201901,201910,1)) +
  coord_flip() +
  labs(title="근로복지지원 현황",caption="공공데이터포털",x="기준년월",y="지원자수")

#2014년 대덕구 보궐선거 개표결과
vote <- read.csv(file="2014년 대덕구 보궐선거 개표결과.csv", header=T)
for(i in 3:9){
  vote[,i] <- as.integer(gsub(",","",vote[,i]))
}

vote <- vote[2:71,]
vote <- rbind(vote[1:3,],filter(vote,vote$투표구명=="소계"))
gggg1 <- treemap(vote,
        index="읍면동명",
        vSize="새누리당.정용기",
        type="index"
)

gggg2 <- treemap(vote,
                 index="읍면동명",
                 vSize="새정치민주연합.박영순",
                 type="index"
)

vote1 <- select(vote, 읍면동명,새누리당.정용기)
vote2 <- select(vote, 읍면동명,새정치민주연합.박영순)

person <- c(rep("새누리당 정용기",times=nrow(vote1)))
vote1 <- cbind(vote1,person)
person <- c(rep("새정치민주연합 박영순",times=nrow(vote2)))
vote2 <- cbind(vote2,person)

colnames(vote1) <- c("읍면동명","득표수","후보자")
colnames(vote2) <- c("읍면동명","득표수","후보자")

vote <- rbind(vote1,vote2)


ggplot(vote, aes(fill=읍면동명, y=득표수, x=후보)) + 
  geom_bar(position="stack", stat="identity")
