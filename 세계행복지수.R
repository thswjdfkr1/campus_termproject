rm(live)

live <- read.csv("C://Users/thswj/OneDrive/바탕 화면/Cost_of_Living_Index_2022.csv")
head(live)
str(live)
live <- live[,-c(1,2,5)]
head(live)
table(is.na(live))
live<-na.omit(live)
head(live)
str(live)
pairs(live) #변수 간 관계를 산점도로 표현
cor(live) #변수 간 상관계수 확인

df1 <- live[,c(1,2)] #1열,2열 선택
df2 <- live[,c(1,3)]
df3 <- live[,c(1,4)]
df4 <- live[,c(1,5)]
plot(df1) #1열,2열 산점도
plot(df2)
plot(df3)
plot(df4)

cl_live <- kmeans(live, center = 10) #kmeans 군집화
cl_live
str(cl_live)
plot(live, col=cl_live$cluster) #kmeans 군집화 산점도
cl_live$cluster 
table(cl_live$cluster) #군집에 할당된 데이터 갯수
cl_live$size
plot(live, col=cl_live$cluster, pch=1) #pch는 산점도 모양, 색깔은 군집별로

cl_live$whithnss #군집내 제곱함
cl_live$tot.withinss #군내변동의 총합

wss <- 0 #군집 적합 수 구하기
wss2 <- 0
for (i in 1:10){
  wss[i] <- sum(kmeans(live, center=i)$withinss)
  wss2[i] <- kmeans(live, center=i)$tot.withinss
}
wss
wss2
plot(wss, type='b') #엘보우 포인트 8
plot(wss2, type='b')

dist_live <- dist(live, method = "euclidean") #유클리드 거리리
library(cluster)
(sil = silhouette(cl_live$cluster, dist_live)) #실루엣 함수 
sil #1열 군집번호, 2열 인접군집, 3열 실루엣 계수
sil[,3]
mean(sil[,3]) #실루엣 계수 평균
plot(sil) #실루엣 계수 확인

cl_live2 <- kmeans(live, center=10)
sil2 <- silhouette(cl_live2$cluster, dist_live)
mean(sil2[,3])

avg_sil <- 0 #실루엣 계수 평균으로 적합 군집수 확인
for(i in 1:10){
  cl_live2 <- kmeans(live, center=i)
  sil2 <- silhouette(cl_live2$cluster, dist_live)
  avg_sil[i] <- mean(sil2[3])
}

avg_sil #na는 초기값
plot(avg_sil, type='b')

final_live <- kmeans(live, center= 9) #최종 군집 계수로 kmeans 군집화
final_live
head(live)
plot(live, col=cl_live$cluster)
plot(live$Cost.of.Living.Index,live$Rent.Index, col=cl_live$cluster)
final_live$whithnss #군집내 제곱함
final_live$tot.withinss #군내변동의 총합

live$cl_num <- final_live$cluster #할당된 군집을 마지막 열에 추가 
head(live)

#sub1 <- live[live$cl_num==1,]
#sub2 <- live[live$cl_num==2,]

sub1 <- subset(live, cl_num==1) #군집이 1인 애들만 추출
sub2 <- subset(live, cl_num==2)
sub3 <- subset(live, cl_num==3)
sub4 <- subset(live, cl_num==4)
sub5 <- subset(live, cl_num==5)
sub6 <- subset(live, cl_num==6)
sub7 <- subset(live, cl_num==7)
sub8 <- subset(live, cl_num==8)
sub9 <- subset(live, cl_num==9)
summary(sub1)
summary(sub2)
table(cl_num)

boxplot(sub1$Rent.Index) #군집이 1인 렌트지수
boxplot(sub2$Rent.Index)

par(mfrow=c(1,2)) #화면 분할로 그래프 비교
boxplot(sub1$Rent.Index, ylim=c(10,40))
boxplot(sub2$Rent.Index, ylim=c(10,40))

par(mfrow=c(1,1))
boxplot(live$Rent.Index ~ as.factor(live$cl_num)) #렌트지수 군집별 한번에 비교
attach(live)
boxplot(Rent.Index ~ as.factor(live$cl_num))
boxplot(Cost.of.Living.Index ~ as.factor(live$cl_num))
boxplot(Groceries.Index ~ as.factor(live$cl_num))
boxplot(Restaurant.Price.Index ~ as.factor(live$cl_num))
boxplot(Local.Purchasing.Power.Index ~ as.factor(live$cl_num))

