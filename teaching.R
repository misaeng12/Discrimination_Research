library(dplyr)
library(ggplot2)

data <- read.csv("C:/Users/user/Misaeng/폴더/교직부/data.csv")
data <- data[2:359,] %>% select(학교종류, X9, X10, X11, X12, X13, X14, X16)
data[!complete.cases(data),]
data <- data[-285,]
n <- nrow(data)

data.m <- filter(data, 학교종류==2)  #중학교
data.h <- filter(data, 학교종류==3)  #고등학교
n.m <- nrow(data.m); n.h <- nrow(data.h)

t <- table(data.m$X16); df <- data.frame(n=t, p=round(t/n.m*100, 1))[,-3]; df
t <- table(data.h$X16); df <- data.frame(n=t, p=round(t/n.h*100, 1))[,-3]; df
t <- table(data$X16); df <- data.frame(n=t, p=round(t/n*100, 1))[,-3]; df
sum(df$p.Freq)


data2 <- read.csv("C:/Users/user/Misaeng/폴더/교직부/data2018.csv")
data2 <- data2[2:331,] %>% select(학교종류, X9, X10, X11, X12, X13, X14, X16)

data <- rbind(data, data2) %>% mutate(연도=as.factor(rep(c(2019, 2018), c(n, nrow(data2)))))

ggplot(data, aes(x=X9, y=..prop.., fill=연도)) + stat_count(position = "dodge2") +
  geom_text(stat="count", aes(label=format(100*..prop.., digits=1, nsmall=1)),
            position=position_dodge(width=1), vjust=-1) +
  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=seq(0, 100, 20), limits=c(0, 1)) +
  scale_x_continuous(breaks=1:5, labels=c("매우불만족", "불만족", "보통", "만족", "매우만족")) +
  labs(title="1. 참관 실습 만족도", y="", x="") + scale_fill_grey() + theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggplot(data, aes(X10, ..prop.., fill=연도)) + stat_count(position = "dodge2") +
  geom_text(stat="count", aes(label=format(100*..prop.., digits=1, nsmall=1)),
            position=position_dodge(width=1), vjust=-1) +
  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=seq(0, 100, 20), limits=c(0, 1)) +
  scale_x_continuous(breaks=1:5, labels=c("매우불만족", "불만족", "보통", "만족", "매우만족")) +
  labs(title="2. 수업 실습 만족도", y="", x="") + scale_fill_grey() + theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggplot(data, aes(X11, ..prop.., fill=연도)) + stat_count(position = "dodge2") +
  geom_text(stat="count", aes(label=format(100*..prop.., digits=1, nsmall=1)),
            position=position_dodge(width=1), vjust=-1) +
  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=seq(0, 100, 20), limits=c(0, 1)) +
  scale_x_continuous(breaks=1:5, labels=c("매우불만족", "불만족", "보통", "만족", "매우만족")) +
  labs(title="3. 학생생활지도 실습 만족도", y="", x="") + scale_fill_grey() + theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggplot(data, aes(X12, ..prop.., fill=연도)) + stat_count(position = "dodge2") +
  geom_text(stat="count", aes(label=format(100*..prop.., digits=1, nsmall=1)),
            position=position_dodge(width=1), vjust=-1) +
  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=seq(0, 100, 20), limits=c(0, 1)) +
  scale_x_continuous(breaks=1:5, labels=c("매우불만족", "불만족", "보통", "만족", "매우만족")) +
  labs(title="4. 학급경영 실습 만족도", y="", x="") + scale_fill_grey() + theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggplot(data, aes(X13, ..prop.., fill=연도)) + stat_count(position = "dodge2") +
  geom_text(stat="count", aes(label=format(100*..prop.., digits=1, nsmall=1)),
            position=position_dodge(width=1), vjust=-1) +
  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=seq(0, 100, 20), limits=c(0, 1)) +
  scale_x_continuous(breaks=1:5, labels=c("매우불만족", "불만족", "보통", "만족", "매우만족")) +
  labs(title="5. 실무 실습 만족도", y="", x="") + scale_fill_grey() + theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggplot(data, aes(X14, ..prop.., fill=연도)) + stat_count(position = "dodge2") +
  geom_text(stat="count", aes(label=format(100*..prop.., digits=1, nsmall=1)),
            position=position_dodge(width=1), vjust=-1) +
  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=seq(0, 100, 20), limits=c(0, 1)) +
  scale_x_continuous(breaks=1:5, labels=c("매우불만족", "불만족", "보통", "만족", "매우만족")) +
  labs(title="6. 교육실습 전반에 대한 만족도", y="", x="") + scale_fill_grey() + theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggplot(data, aes(X16, ..prop.., fill=연도)) + stat_count(position = "dodge2") +
  geom_text(stat="count", aes(label=format(100*..prop.., digits=1, nsmall=1)),
            position=position_dodge(width=1), vjust=-1) +
  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=seq(0, 100, 20), limits=c(0, 1)) +
  scale_x_continuous(breaks=1:7, labels=c("수업", "학생생활지도", "학급경영", "행정업무", "학생들과의\n소통", "교사들과의\n관계", "기타")) +
  labs(title="7. 교육실습 중 가장 어려웠던 점", y="", x="") + scale_fill_grey() + theme_bw() +
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank())
