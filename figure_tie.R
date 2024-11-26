library(tidyverse)
library(ggplot2)
library(readxl)
library(ggpubr)

dat = read_xlsx("typeIerror.xlsx")
  
dat = dat %>%
  mutate(Method = factor(Method, levels = c("permutation", "wchisq", "chisq1"))) 

p1 = ggplot(dat%>%filter(setting=="Null setting 1"), aes(x=n, y=typeIerror, group=Method))+
  scale_x_continuous(breaks=c(32, 64, 96, 128))+
  scale_y_continuous(limits=c(0, 0.15))+
  geom_line(aes(col=Method), lwd = 1)+
  xlab("sample size") +
  ylab("empirical power") +
  facet_wrap(~setting, nrow=1, ncol=1, scales="free_x")+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")

p2 = ggplot(dat%>%filter(setting=="Null setting 2"), aes(x=n, y=typeIerror, group=Method))+
  scale_x_continuous(breaks=c(50, 100, 150, 200))+
  scale_y_continuous(limits=c(0, 0.15))+
  geom_line(aes(col=Method), lwd = 1)+
  xlab("sample size") +
  ylab("empirical power") +
  facet_wrap(~setting, nrow=1, ncol=1, scales="free_x")+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")


p3 = ggplot(dat%>%filter(setting=="Null setting 3"), aes(x=n, y=typeIerror, group=Method))+
  scale_x_continuous(breaks=c(32, 64, 96, 128))+
  scale_y_continuous(limits=c(0, 0.15))+
  geom_line(aes(col=Method), lwd = 1)+
  xlab("sample size") +
  ylab("empirical power") +
  facet_wrap(~setting, nrow=1, ncol=1, scales="free_x")+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")

p4 = ggplot(dat%>%filter(setting=="Null setting 4"), aes(x=n, y=typeIerror, group=Method))+
  scale_x_continuous(breaks=c(50, 100, 150, 200))+
  scale_y_continuous(limits=c(0, 0.15))+
  geom_line(aes(col=Method), lwd = 1)+
  xlab("sample size") +
  ylab("empirical power") +
  facet_wrap(~setting, nrow=1, ncol=1, scales="free_x")+
  theme(legend.title = element_blank(), text = element_text(size = 20), element_line(size = 1),
        legend.position="right")

ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "right")


