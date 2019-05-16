data <- read.csv("data.csv")
attach(data)
library(tidyverse)

### 학력 3종 변수 생성 (1:고졸이하, 2:대졸이상, 3:대학원이상)
DQ3_2 <- c()
for(i in 1:nrow(data)){
  DQ3_2[i] <- ifelse(DQ3_1[i]==3, 2, ifelse(DQ3_1[i]==4, 3, 1))
}
data$DQ3_2 <- as.factor(DQ3_2)

### 종교 3종 변수 생성 (1:기독교, 2:타종교, 3:무종교)
SQ4_5 <- c()
for(i in 1:nrow(data)){
  SQ4_5[i] <- ifelse(SQ4[i]==3, 1, ifelse(SQ4[i]==1, 3, 2))
}
data$SQ4_5 <- as.factor(SQ4_5)

### 성차별 경험 변수 생성 (1:없음, 2:없음, 3:잘모르겠음)
Q3.4 <- c()
for(i in 1:nrow(data)){
  if(Q3[i]==1) Q3.4[i] <- ifelse(Q4_M1[i]==1, 1, 2)
  else Q3.4[i] <- ifelse(Q3[i]==2, 2, 3)
}
data$Q3.4 <- as.factor(Q3.4)

### 반페미니즘 설교 또는 메시지 경험 변수 생성 (1:있음, 2:없음)
Q20_4 <- c(); Q20_5 <- c()
for(i in 1:nrow(data)){
  Q20_4[i] <- ifelse(Q20_4_1[i]==1 | Q20_4_2[i]==1 | Q20_4_3[i]==1, 2, 1)
  Q20_5[i] <- ifelse(Q20_5_1[i]==1 | Q20_5_2[i]==1 | Q20_5_3[i]==1, 2, 1)
}
data$Q20_4 <- as.factor(Q20_4)
data$Q20_5 <- as.factor(Q20_5)

write.csv(data, "data.csv")



# 1. 표본의 인구사회적 특성
table(SQ1)                                     # 성별
table(SQ3)                                     # 연령
t <- table(DM3); c(t[c(1, 2)], sum(t[3:7]))    # 거주지역(1:서울, 2:인천/경기, 3:수도권)
table(DQ1_1)                                   # 혼인상태(3)
table(DQ3_2)                                   # 학력(1:고졸이하, 2:대졸이상, 3:대학원이상)
t <- table(DQ4); c(sum(t[1:2]), t[3], sum(t[4:5]), sum(t[6:7]), t[8:11])  # 직업(8)
t<- table(DM6); c(t[1:2], sum(t[3:4]), t[5:6]) # 가구소득(5)
table(DM11)                                    # 이념성향(보수, 중도, 진보) 
table(DM8)                                     # 종교(6)



# 5. 기독교인의 차별 경험과 여성 혐오 인식

### <표16> 종교별 인구사회적 특성
d1 <- filter(data, SQ4_5==1)
d2 <- filter(data, SQ4_5==2)
d3 <- filter(data, SQ4_5==3)
(n1 <- nrow(d1))
(n2 <- nrow(d2))
(n3 <- nrow(d3))

table(d1$SQ1)/n*100                                      # 성별
table(d1$SQ3)/n*100                                      # 연령
t <- table(d1$DM3)/n*100; c(t[1:2], sum(t[3:7]))         # 거주지역(1:서울, 2:인천/경기, 3:수도권)
table(d1$DQ1_1)/n*100                                    # 혼인상태(3)
table(d1$DQ3_2)/n*100                                    # 학력(1:고졸이하, 2:대졸이상, 3:대학원이상)
t <- table(d1$DQ4)/n*100; c(sum(t[1:2]), t[3], sum(t[4:5]), sum(t[6:7]), t[8:11]) # 직업(8)
t <- table(d1$DM6)/n*100; c(t[1:2], sum(t[3:4]), t[5:6]) # 가구소득(5)
table(d1$DM11)/n*100                                     # 이념성향(보수, 중도, 진보) 


### <표17> 종교집단별 차별 경험
t <- table(Q3); c(t[1:2], sum(t[1:2]))
(X <- cbind(table(d1$Q3)[1:2]/n1, table(d2$Q3)[1:2]/n2, table(d3$Q3)[1:2]/n3))
colSums(X)
chisq.test(X)

### <표18> 기독교인의 차별 경험
detach(data)
attach(d1)
d1.1 <- filter(d1, Q3==1); d1.2 <- filter(d1, Q3==2)
(X <- cbind(table(d1.1$Q22)/nrow(d1.1), table(d1.2$Q22)/nrow(d1.2)))
chisq.test(X)

### <표19> 종교집단별 차별 해소 노력 인식 차이
summary(aov(data=data, RQ5 ~ SQ4_5))

### <표20> 기독교인의 차별 해소 노력 인식 차이
t.test(data=d1, RQ5 ~ as.factor(SQ1))
summary(aov(data=d1, RQ5 ~ as.factor(Q22)))
group_by(d1, as.factor(DQ3_2)) %>% summarise(mean(RQ5))

### <표21> 종교집단별 역차별 인식
(t <- group_by(data, SQ4_5, Q8) %>% count() %>% spread(SQ4_5, n))
(X <- cbind(t[,2]/n1, t[,3]/n2, t[,4]/n3))
chisq.test(X)

### <표22> 기독교인의 역차별 인식
(t <- group_by(d1, Q8, Q22) %>% count() %>% spread(Q8, n))
(X <- cbind(t[,2]/sum(t[,2]), t[,3]/sum(t[,3]), t[,4]/sum(t[,4]), t[,5]/sum(t[,5])))
chisq.test(X)

### <표23>
(t <- group_by(data, SQ4_5, Q3.4) %>% count() %>% spread(SQ4_5, n))
(X <- cbind(t[c(1,2),2]/sum(t[,2]), t[c(1,2),3]/sum(t[,3]), t[c(1,2),4]/sum(t[,4])))
chisq.test(X)

### <표24>
(t <- group_by(d1, Q3.4, Q22) %>% count() %>% spread(Q3.4, n))
(X <- cbind(t[,2]/sum(t[,2]), t[,3]/sum(t[,3])))
chisq.test(X)

### <표25>
t <- group_by(d1, Q20_4, Q22) %>% count() %>% spread(Q20_4, n)
X <- cbind(t[,2]/sum(t[,2]), t[,3]/sum(t[,3]))
chisq.test(X)

t <- group_by(d1, Q20_5, Q22) %>% count() %>% spread(Q20_5, n)
X <- cbind(t[,2]/sum(t[,2]), t[,3]/sum(t[,3]))
chisq.test(X)

### <표26> 종교집단별 여성혐오 인식
summary(aov(data=data, FACTOR3 ~ SQ4_5))
group_by(data, SQ4_5) %>% summarise(mean=mean(FACTOR), SD=sd(FACTOR))

### <표27> 기독교 경험과 혐오 인식
t.test(data=d1, FACTOR2 ~ as.factor(Q20_5))
summary(aov(data=d1, FACTOR2 ~ as.factor(Q22)))


### 기독교인의 여성혐오 영향 요인에 대한 다중회귀분석

data$SQ1 <- as.factor(data$SQ1)
data$SQ3 <- as.factor(data$SQ3)
data$DQ1_1 <- as.factor(data$DQ1_1)
data$DQ3_2 <- as.factor(data$DQ3_2)
data$Q20_1_1 <- as.factor(data$Q20_1_1)
data$Q20_2 <- as.factor(data$Q20_2)
data$Q3.4 <- relevel(as.factor(data$Q3.4), ref=2)
data$Q20_4 <- relevel(as.factor(data$Q20_4), ref=2)
data$Q20_5 <- relevel(as.factor(data$Q20_5), ref=2)
d1 <- filter(data, SQ4_5==1)

lm1 <- lm(FACTOR ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6, d1)
summary(lm1)

lm2 <- lm(FACTOR3 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 +
            RQ11_1 + RQ1_1 + Q3.4, d1)
summary(lm2)

lm3 <- lm(FACTOR ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 +
            RQ11_1 + RQ1_1 + Q3.4 +
            Q20_1_1 + Q20_2 + Q22 + Q23 + Q20_4 + Q20_5, d1)
summary(lm3)

lm4 <- lm(FACTOR3 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 +
            RQ11_1 + RQ1_1 + Q3.4 +
            Q20_1_1 + Q20_2 + Q22 + Q23 + Q20_4 + Q20_5 +
            RQ20_3 + RQ24 + RQ25 + RQ28, d1)
summary(lm4)


## stepwise

lm <- lm(FACTOR ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4 + Q20_1_1 +
          Q20_2 + Q22 + Q23 + Q20_4 + Q20_5 + RQ20_3 + RQ24 + RQ25 + RQ28, d1)
lm.step <- step(lm)
summary(lm.step)

lm1 <- lm(FACTOR1 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4 + Q20_1_1 +
           Q20_2 + Q22 + Q23 + Q20_4 + Q20_5 + RQ20_3 + RQ24 + RQ25 + RQ28, d1)
lm1.step <- step(lm1)
summary(lm1.step)

lm2 <- lm(FACTOR2 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4 + Q20_1_1 +
           Q20_2 + Q22 + Q23 + Q20_4 + Q20_5 + RQ20_3 + RQ24 + RQ25 + RQ28, d1)
lm2.step <- step(lm2)
summary(lm2.step)

lm3 <- lm(FACTOR3 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4 + Q20_1_1 +
           Q20_2 + Q22 + Q23 + Q20_4 + Q20_5 + RQ20_3 + RQ24 + RQ25 + RQ28, d1)
lm3.step <- step(lm3)
summary(lm3.step)


## 종교별 차이 비교 (교회 관련 변수 제외)

# 기독교
lm <- lm(FACTOR ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d1)
lm.step <- step(lm); summary(lm.step)
lm1 <- lm(FACTOR1 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d1)
lm1.step <- step(lm1); summary(lm1.step)
lm2 <- lm(FACTOR2 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d1)
lm2.step <- step(lm2); summary(lm2.step)
lm3 <- lm(FACTOR3 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d1)
lm3.step <- step(lm3); summary(lm3.step)

# 타종교
d2 <- filter(data, data$SQ4_5==2)
lm <- lm(FACTOR ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d2)
lm.step <- step(lm); summary(lm.step)
lm1 <- lm(FACTOR1 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d2)
lm1.step <- step(lm1); summary(lm1.step)
lm2 <- lm(FACTOR2 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d2)
lm2.step <- step(lm2); summary(lm2.step)
lm3 <- lm(FACTOR3 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d2)
lm3.step <- step(lm3); summary(lm3.step)

# 무종교
d3 <- filter(data, data$SQ4_5==3)
lm <- lm(FACTOR ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d3)
lm.step <- step(lm); summary(lm.step)
lm1 <- lm(FACTOR1 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d3)
lm1.step <- step(lm1); summary(lm1.step)
lm2 <- lm(FACTOR2 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d3)
lm2.step <- step(lm2); summary(lm2.step)
lm3 <- lm(FACTOR3 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4, d3)
lm3.step <- step(lm3); summary(lm3.step)

# 전체 data (종교 변수 포함)
data$SQ4_5 <- as.factor(data$SQ4_5)
lm <- lm(FACTOR ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4 + SQ4_5, data)
lm.step <- step(lm); summary(lm.step)
lm1 <- lm(FACTOR1 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4 + SQ4_5, data)
lm1.step <- step(lm1); summary(lm1.step)
lm2 <- lm(FACTOR2 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4 + SQ4_5, data)
lm2.step <- step(lm2); summary(lm2.step)
lm3 <- lm(FACTOR3 ~ SQ1 + SQ3 + DQ1_1 + DQ3_2 + DM6 + RQ11_1 + RQ1_1 + Q3.4 + SQ4_5, data)
lm3.step <- step(lm3); summary(lm3.step)
