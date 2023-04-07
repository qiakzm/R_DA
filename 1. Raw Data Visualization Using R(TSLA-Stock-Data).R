library(ggplot2)
library(dplyr)
library(reshape)
library(tseries)
library(cowplot)
library(forecast)
TSLA = read.csv("TSLA.csv")
TSLA$Date = as.Date(TSLA$Date)

#1-1. 테슬라 주식의 날짜별 가격들 시각화. (거래량은 단위가 다르기 때문에 제외)
TSLA %>%
  select(-Volume) %>%
  melt(id.vars = c("Date")) %>%
  ggplot()+
  geom_point(aes(x = Date, y = value, col = variable),
             alpha = 0.5)+
  geom_line(aes(x = Date, y = value, col = variable, group = variable),
            alpha = 0.8)+
  xlab("Date") + ylab("Stock Price")+
  labs(col = "Types of Price")+
  theme_bw()+
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2,0.8))

#1-2. 테슬라 주식의 거래량(Volume) 시각화.
TSLA %>%
  select(Date, Volume) %>%
  ggplot() +
  geom_point(aes(x = Date, y = Volume),
             alpha = 0.5)+
  geom_line(aes(x = Date, y = Volume),
            alpha = 0.8)+
  xlab("Date") + ylab("Volume") +
  labs(col = "Types of Price")+
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"))

#1-3. 1-1번과 1-2번을 동시에 표현
A = TSLA %>%
  select(-Volume) %>%
  melt(id.vars = c("Date")) %>%
  ggplot()+
  geom_point(aes(x = Date, y = value, col = variable),
             alpha = 0.5)+
  geom_line(aes(x = Date, y = value, col = variable, group = variable),
            alpha = 0.8)+
  xlab("Date") + ylab("Stock Price")+
  labs(col = "Types of Price")+
  theme_bw()+
  theme(text = element_text(size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.2,0.8))
B = TSLA %>%
  select(Date, Volume) %>%
  ggplot() +
  geom_point(aes(x = Date, y = Volume),
             alpha = 0.5)+
  geom_line(aes(x = Date, y = Volume),
            alpha = 0.8)+
  xlab("Date") + ylab("Volume") +
  labs(col = "Types of Price")+
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"))
cowplot::plot_grid(A,B, ncol = 1, labels= c("A","B"))


#2. 지수평활법(Moving Average) 시각화

Smoothing = function(x, interval){
  
  ts_x = ts(x)
  Sm = stats::filter(ts_x, filter = rep(1/interval, interval))
  #dplyr::filter()와 명령어가 충돌하는 것을 방지하기 위함.
  #여러변수에 대해 한번에 처리하기 위한 함수화.
  
  return(Sm)
}

#2-1. Open
Open_Week = Smoothing(x = TSLA$Open,
                      interval = 7)
DATE = TSLA$Date

ggplot(NULL)+
  geom_point(aes(x = DATE, y = Open_Week))+
  theme_bw()+
  theme(text = element_text(size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.2,0.5))

#2-2. 30 days Smoothing
L_Smoothing = lapply(TSLA[,-grep("Date", colnames(TSLA))],
                     function(x, interval) Smoothing(x,30))

L_Smoothnig = as.data.frame(L_Smoothing)

head(L_Smoothing, n = 30)

L_Smoothing$Date = DATE

L_Smoothing %>%
  na.omit() %>%
  select(-Volume) %>%
  melt(id.vars = c("Date")) %>%
  ggplot() +
  geom_point(aes(x = Date, y = value, col = variable),
             alpha = 0.5)+
  geom_line(aes(x = Date, y = value, col = variable, group = variable),
            alpha = 0.8)+
  xlab("Date") + ylab("Stock Price")+
  ggtitle("30 days Smoothing (Moving Average)")+
  labs(col = "Types of Price")+
  theme_bw()+
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.25,0.7))

#2-3. N days Smoothing , asssign 활용하기

INTERVAL = c(30,60,90,120,365)

for(i in INTERVAL){
  
  DF = lapply(TSLA[,-grep("Date",colnames(TSLA))],
              function(x, interval) Smoothing(x,i))
  
  DF = as.data.frame(DF)
  assign(paste0("L_Smoothing_",i),DF)
}

TSLA2 = data.frame(
  
  DATE = TSLA$Date,
  Open = TSLA$Open,
  Open_30 = L_Smoothing_30$Open,
  Open_60 = L_Smoothing_60$Open,
  Open_90 = L_Smoothing_90$Open,
  Open_120 = L_Smoothing_120$Open,
  Open_365 = L_Smoothing_365$Open
)

TSLA2 %>%
  melt(id.vars = c("DATE")) %>%
  ggplot()+
  geom_line(aes(x = DATE, y = value, col = variable, group = variable), size = 1.2) +
  theme_bw()+
  xlab("Date") + ylab("Stock Price")+
  ggtitle("Smoothing (Moving Average)")+
  labs(col = "Smoothing Days")+
  theme_bw()+
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2, 0.6))

TSLA2 %>%
  melt(id.vars = c("DATE")) %>%
  ggplot() +
  geom_line(aes(x = DATE, y = value, col = variable, group = variable), size = 1.2)+
  theme_bw()+
  xlab("Date") + ylab("Stock Price")+
  ggtitle("Smoothing (Moving Average)")+
  labs(col = "Smoothing Days")+
  guides(col = FALSE) +
  theme_bw()+
  theme(text = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 90))+
  facet_wrap(~variable)

#3. 시계열 분해

Decomposition = function(x){
  
  TS_X = ts(x, frequency = 365)
  
  Add = decompose(TS_X, type = "additive")
  Multi = decompose(TS_X, type = "multiplicative")
  
  Result = list()
  Result$Add = Add
  Result$Multi = Multi
  
  return(Result)
}

TD = Decomposition(x = TSLA$Open)

#Additive
autoplot(TD$Add)+
  theme_bw()+
  theme(text = element_text(size = 15, face = "bold"))

#Multi
autoplot(TD$Multi)+
  theme_bw()+
  theme(text = element_text(size = 15, face = "bold"))

#4. Waterfall Chart

Difference = function(x){
  
  Diff_Vector = c()
  Diff_Vector[1] = 0
  
  for(k in 2:length(x)){
    
    Diff = x[k] - x[k-1]
    
    Diff_Vector[k] = Diff
  }
  
  return(Diff_Vector)
  
}

Open_Diff = Difference(x = TSLA$Open)

WATER = data.frame(
  DATE = TSLA$Date,
  Open = Open_Diff
)

WATER$Increase = ifelse(WATER$Open >= 0, "Plus", "Minus")

WATER %>%
  mutate(Increase = factor(Increase, levels = c("Plus", "Minus"))) %>%
  ggplot() + 
  geom_bar(aes(x = DATE, y = Open, fill = Increase), stat = 'identity')+
  scale_fill_manual(values = c("red","royalblue"))+
  theme_bw()+
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = "bottom")

WATER[(nrow(WATER)-100):nrow(WATER),] %>%
  mutate(Increase = factor(Increase, levels = c("Plus", "Minus"))) %>%
  ggplot() +
  geom_bar(aes(x = DATE, y = Open, fill = Increase), stat = 'identity')+
  scale_fill_manual(values = c("red","royalblue")) +
  theme_bw()+
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = "bottom")
