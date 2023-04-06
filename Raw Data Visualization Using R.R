library(ggplot2)
library(dplyr)
library(reshape)
library(tseries)
library(cowplot)
library(forecast)
TSLA = read.csv("TSLA.csv")
TSLA$Date = as.Date(TSLA$Date)
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
