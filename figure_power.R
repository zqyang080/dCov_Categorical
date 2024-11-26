library(tidyverse)
library(ggplot2)
library(readxl)
library(ggpubr)

dat = read_xlsx("power.xlsx")
  
dat = dat %>%
  mutate(Method = factor(Method, levels = c("permutation", "wchisq", "chisq1"))) 

p1 = ggplot(dat%>%filter(setting=="Alternative setting 1"), aes(x=n, y=power, group=Method))+
  scale_x_continuous(breaks=c(32, 64, 96, 128))+
  scale_y_continuous(limits=c(0, 1))+
  geom_line(aes(col=Method), lwd = 0.8)+
  xlab("sample size") +
  ylab("empirical power") +
  facet_wrap(~setting, nrow=1, ncol=1, scales="free_x")+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")

p2 = ggplot(dat%>%filter(setting=="Alternative setting 2"), aes(x=n, y=power, group=Method))+
  scale_x_continuous(breaks=c(50, 100, 150, 200))+
  scale_y_continuous(limits=c(0, 1))+
  geom_line(aes(col=Method), lwd = 0.8)+
  xlab("sample size") +
  ylab("empirical power") +
  facet_wrap(~setting, nrow=1, ncol=1, scales="free_x")+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")


p3 = ggplot(dat%>%filter(setting=="Alternative setting 3"), aes(x=n, y=power, group=Method))+
  scale_x_continuous(breaks=c(32, 64, 96, 128))+
  scale_y_continuous(limits=c(0, 1))+
  geom_line(aes(col=Method), lwd = 0.8)+
  xlab("sample size") +
  ylab("empirical power") +
  facet_wrap(~setting, nrow=1, ncol=1, scales="free_x")+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")

p4 = ggplot(dat%>%filter(setting=="Alternative setting 4"), aes(x=n, y=power, group=Method))+
  scale_x_continuous(breaks=c(50, 100, 150, 200))+
  scale_y_continuous(limits=c(0, 1))+
  geom_line(aes(col=Method), lwd = 0.8)+
  xlab("sample size") +
  ylab("empirical power") +
  facet_wrap(~setting, nrow=1, ncol=1, scales="free_x")+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")

p5 = ggplot(dat%>%filter(setting=="Alternative setting 5"), aes(x=n, y=power, group=Method))+
  scale_x_continuous(breaks=c(32, 64, 96, 128))+
  scale_y_continuous(limits=c(0, 1))+
  geom_line(aes(col=Method), lwd = 0.8)+
  xlab("sample size") +
  ylab("empirical power") +
  facet_wrap(~setting, nrow=1, ncol=1, scales="free_x")+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")

p6 = ggplot(dat%>%filter(setting=="Alternative setting 6"), aes(x=n, y=power, group=Method))+
  scale_x_continuous(breaks=c(50, 100, 150, 200))+
  scale_y_continuous(limits=c(0, 1))+
  geom_line(aes(col=Method), lwd = 0.8)+
  xlab("sample size") +
  ylab("empirical power") +
  facet_wrap(~setting, nrow=1, ncol=1, scales="free_x")+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")

ggarrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2, common.legend = TRUE, legend = "right")


