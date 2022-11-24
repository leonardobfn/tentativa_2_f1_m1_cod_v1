rm(list=ls())
require(tidyverse)
list.files("estimations_predicts",pattern = "txt")
#path_monte_carlo <- "estimations_predicts/estimation_mc.txt"
path_monte_carlo <- "estimations_predicts/estimation_mc.txt"
estimation_mc <- read.table(path_monte_carlo,
                         col.names = c("id","estimate","par"))

estimation_mc %>% filter(par=="(Intercept)_delta")
na <-  estimation_mc %>% pull(estimate)
(which(is.na(na)==T) %>% length())/12
TT <- nrow(estimation_mc)/12
TT
estimation_mc %>% mutate(t=rep(seq(1,TT),each=12) ) %>% group_by(par) %>% summarise(min(estimate,na.rm = T))

fig <- estimation_mc %>% mutate(t=rep(seq(1,TT),each=12) ) %>%
  dplyr::group_split(par) %>%
  purrr::map(
  ~ggplot(.,aes(x=t,y=estimate)) +
  geom_line()+
    theme_bw()+
    facet_wrap(~par,scales = "free")+
    #geom_hline(data = d,aes(yintercept = kp))+
    #geom_hline(yintercept = q)+
    theme(legend.position = "none",axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig

fig <- estimation_mc %>% mutate(t=rep(seq(1,TT),each=12) ) %>%
  dplyr::group_split(par) %>%
  purrr::map(
    ~ggplot(.,aes(estimate)) +
      geom_histogram()+
      theme_bw()+
      facet_wrap(~par,scales = "free")+
      #geom_hline(data = d,aes(yintercept = kp))+
      #geom_hline(yintercept = q)+
      theme(legend.position = "none",axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig

path_estimation <- "estimations_predicts/estimations.rds"
results <- readRDS(path_estimation)
estimation <- c(results$ar$Estimate,
  results$kl$Estimate,
  results$delta$Estimate,
  results$alpha$Estimate)
al <- .10/2
est <- estimation_mc %>% group_by(par,id) %>%
  summarise(Mean=mean(estimate,na.rm=T,trim=0),
            Sd=sd(estimate,na.rm=T),
            Interval=t(quantile(estimate,c(al,1-al),na.rm=T)),
            min=min(estimate,na.rm = T),
            max=max(estimate,na.rm = T)) %>%
  arrange(id) %>% data.frame() %>% select(-id)


est1 <- est %>% mutate(Estimate=estimation) %>% select(par,Estimate,Mean,everything())
est1
certo <- which(est1$Estimate > est1$Interval[,1] & est1$Estimate < est1$Interval[,2])
est1[certo,]
est1[-certo,]

#log.like----------

path_log.like<- "estimations_predicts/log_like.txt"
log.like <- read.table(path_log.like,
                            col.names = c("id","log_like"))

hist(log.like$log_like)
