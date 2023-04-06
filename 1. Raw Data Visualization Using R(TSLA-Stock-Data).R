library(ggplot2)
library(dplyr)
library(reshape)
library(tseries)
library(cowplot)
library(forecast)
TSLA = read.csv("TSLA.csv")
TSLA$Date = as.Date(TSLA$Date)

#1. 테슬라 주식의 날짜별 가격들 시각화. (거래량은 단위가 다르기 때문에 제외)
TSLA %>%
  select(-Volume) %>%
  melt(id.vars = c("Date")) %>%
  ggplot()+
       geom_point(aes(x = Date, y = value, col = variable),
                   +                alpha = 0.5)+
       geom_line(aes(x = Date, y = value, col = variable, group = variable),
                  +               alpha = 0.8)+
       xlab("Date") + ylab("Stock Price")+
       labs(col = "Types of Price")+
       theme_bw()+
       theme(text = element_text(size = 15, face = "bold"),
             legend.position = c(0.2,0.8))

#2. 테슬라 주식의 거래량(Volume) 시각화.
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
 
#3. 1번과 2번을 동시에 표현
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
