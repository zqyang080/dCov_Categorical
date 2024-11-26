# setwd("/users/qingyangzhang/desktop/DCtest/simulation/")
library(tidyverse)
library(ggplot2)
library(readxl)

n25 = read.csv("p10n25.csv")
n50 = read.csv("p10n50.csv")
n75 = read.csv("p10n75.csv")
n100 = read.csv("p10n100.csv")
  
dat = bind_rows(n25, n50, n75, n100) %>%
  mutate(n = factor(n, levels = c("25", "50", "75", "100"), label = c("n=25", "n=50", "n=75", "n=100"))) 

ggplot(dat, aes(x=1-spec, y=sens, group=method))+
  scale_x_continuous(limit = c(0, 1), breaks=seq(0,1,0.2))+
  geom_line(aes(col=method), lwd = 1)+
  xlab("1-specificity") +
  ylab("sensitivity") +
  facet_wrap(~n, nrow=2, ncol=2)+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")


