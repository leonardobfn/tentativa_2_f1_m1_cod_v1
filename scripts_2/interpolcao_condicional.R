rm(list = ls())
# Packages -------
pack <-
  c("tidyverse",
    "extraDistr",
    "devtools",
    "Formula",
    "tictoc",
    "betareg",
    "cowplot",
    "stabledist")
# Instalção dos pacotes
package.check <- lapply(
  pack,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# loading my functions
#source("R/rstable.R")
load_all()
# Estimativa para yt(s0) - escolhendo trim -----------------------------------------------

data.aux <- readRDS("scripts_2/1-estimations_Barcelos.rds")
city <- data.aux$data.complete %>% group_by(Group, City) %>% pull(City) %>% unique
results.full <- NULL
value <-mse <- estt<-NULL
mtrim=seq(0.15,0.25,0.01) # grade de valores para trimmed
resul=NULL
# BB=10
# for(l in 1:BB){
# for(i in 1:length(city)){
#   path <- paste0("scripts_2/", i, "-estimations_", city[i], ".rds")
#   for(k in 1:length(mtrim)){
#     data.s0 <- readRDS(path)
#     s0 <- data.s0$s0
#     g <-  data.s0$data.complete %>% filter(City == s0) %>% pull(Group) %>% unique()
#     y.s0 <- data.s0$data.complete %>% filter(City == s0)
#     y.sl.g <- data.s0$data.complete %>% filter(Group==g,City != s0)
#     start <- y.sl.g %>% filter(Year=="2000" & Month=="01") %>% pull(RH) %>% mean
#
#     city.grupos <- unique(y.sl.g$City)
#     delta <- data.s0$DADOS %>% filter(idx==city.grupos[1]) %>%  pull(delta)
#     alfas <- data.s0$alpha$Estimate[as.numeric(str_sub(g,7))]
#
#     link.kl <- data.s0$link.kl
#     XB <- model.matrix(Formula::Formula(data.s0$formula),y.s0,rhs=1)%*%
#       data.s0$kl$Estimate
#     delta <- c(0,delta)
#
#     w0 <- y.s0 %>% pull(weights.w0) %>% unique()
#     n=120
#     N=1000
#
#     yt0est.matrix<- yt0.matrix<-eta_kt0<- r <- matrix(0,n,1)
#     B=30
#
#     yt0<-matrix(0,n,B)
#     for(b in 1:B){
#       #B=2
#       #zm <-  rstable_pos(N,alfas) %>% mean(trim=mtrim[k])
#       zm <- rstable(1000,alpha =alfas,beta = 1,gamma = 1,delta = 0,pm = 1)%>% mean(trim=mtrim[k])
#       H <-w0*zm
#       betaa <- rbeta(1,H,1)
#       yt0.matrix[1] <- start#link.kl$linkinv(data.s0$kl$Estimate[1]+XB[1])
#       for(t in 2:n){
#
#         eta_kt0[t] <- XB[t]+data.s0$ar$Estimate[1]*(link.kl$linkfun(yt0.matrix[t-1])-XB[t-1])+
#           data.s0$ar$Estimate[2]*r[t-1]
#         phit0 <- log(link.kl$linkinv(eta_kt0[t]))/log(1-0.5^(delta[t]))
#         #bq_t=1/delta[t]
#         #aq_t = 1/phit0
#         yt0.matrix[t] <- (1-betaa^delta[t])^phit0
#         #yt0.matrix[t] = (1-(1-0.5)^(1/(bq_t*H)))^(1/aq_t)
#         #a <- log(1-runif(1))*1/H
#         #yt0.matrix[t] = 1-exp(delta[t]*a)
#         #yt0.matrix[t] = (1-exp(-(delta[t]/w0)*(-log(1-0.5))^(1/alfas)))^phit0
#         r[t] <- link.kl$linkfun(yt0.matrix[t]) - eta_kt0[t]
#
#       }
#       yt0[,b] <- yt0.matrix
#     }
#     est <- apply(yt0, 1, median,rm.na=T)
#     est1 <- data.frame(est=est,mtrim=mtrim[k],idx=s0,Group=g,RH=y.s0$RH)
#     #mse.aux1 <- mean(abs(y.s0$RH-est$.)/y.s0$RH)
#     #mse.aux2 <- data.frame(group=y.s0$Group%>%unique(),idx=s0,mse=mse.aux1,mtrim=mtrim[k])
#     #mse <- rbind(mse,mse.aux2)
#     estt <- rbind(estt,est1)
#   }
#
#   #value <- rbind(value,mse)
#
# }
#
# #head(estt)
# tab.resul <-  estt  %>% group_by(Group,mtrim) %>% summarise(mse=mean(abs(est-RH)/RH)) %>%
#   group_by(Group) %>% summarise(mtrim=mtrim[which.min(mse)],mse=min(mse))# melhor trimed por grupo
# aux <- tab.resul %>% data.frame()
#
# resul <- rbind(resul,aux)
# }
# resul %>% filter(Group=="Group 1") %>% pull(mtrim) %>% mean()
# resul %>% filter(Group=="Group 2") %>% pull(mtrim) %>% mean()
# resul %>% filter(Group=="Group 3") %>% pull(mtrim) %>% mean()
# g1 <- estt   %>% group_by(Group,idx) %>% filter(mtrim==tab.resul$mtrim[1],Group=="Group 1") %>%
#   summarise(mse=mean(abs(est-RH)/RH),mtrim=rep(tab.resul$mtrim[1],length(mse)))
# g2 <- estt   %>% group_by(Group,idx) %>% filter(mtrim==tab.resul$mtrim[2],Group=="Group 2") %>%
#   summarise(mse=mean(abs(est-RH)/RH),mtrim=rep(tab.resul$mtrim[2],length(mse)))
# g3 <- estt   %>% group_by(Group,idx) %>% filter(mtrim==tab.resul$mtrim[3],Group=="Group 3") %>%
#   summarise(mse=mean(abs(est-RH)/RH),mtrim=rep(tab.resul$mtrim[3],length(mse)))
#
# rbind(g1,g2,g3)

# Estimativa para yt(s0) -----------------------------------------------
data.aux <- readRDS("scripts_2/1-estimations_Barcelos.rds")
city <- data.aux$data.complete %>% group_by(Group, City) %>% pull(City) %>% unique
results.full <- NULL
rh <- NULL
#mtrim <- tab.resul %>% pull(mtrim)
mtrim <- c(.18,.25,.19,.15,.19,.19,.18,.15,.20,.23,.17,.20,.21,.17)
#mtrim=rep(c(0.20,.20,.20),c(5,3,6))
al = 0.05
for(i in 1:length(city)){
  path <- paste0("scripts_2/", i, "-estimations_", city[i], ".rds")
  data.s0 <- readRDS(path)
  s0 <- data.s0$s0
  g <-data.s0$data.complete %>% filter(City == s0) %>% pull(Group) %>% unique()
  y.s0 <- data.s0$data.complete %>% filter(City == s0)
  y.sl.g <- data.s0$data.complete %>% filter(Group==g,City != s0)
  start <- y.sl.g %>% filter(Year=="2000" & Month=="01") %>% pull(RH) %>% mean
  city.grupos <- unique(y.sl.g$City)
  delta <- data.s0$DADOS %>% filter(idx==city.grupos[1]) %>%  pull(delta)
  alfas <- data.s0$alpha$Estimate[as.numeric(str_sub(g,7))]

  link.kl <- data.s0$link.kl
  XB <- model.matrix(Formula::Formula(data.s0$formula),y.s0,rhs=1)%*%
    data.s0$kl$Estimate
  delta <- c(0,delta)

  w0 <- y.s0 %>% pull(weights.w0) %>% unique()
  n=108
  N=1000
  #mtrim=0.22
  yt0est.matrix<- yt0.matrix<-eta_kt0<- r <- matrix(0,n,1)
  B=50

  yt0<-matrix(0,n,B)
  for(b in 1:B){
    #B=2

    yt0.matrix[1] <- link.kl$linkinv(XB[1])#start#y.s0$RH[1]#link.kl$linkinv(data.s0$kl$Estimate[1]+XB[1]) #y.s0$RH[1]#link.kl$linkinv(data.s0$kl$Estimate[1]+XB[1])#0.8
    for(t in 2:n){

      #zm <-  rstable_pos(N,alfas) %>% mean(trim=mtrim[i])
      zm <- stabledist::rstable(1000,alpha = alfas,beta = 1,gamma = 1,delta = 0,pm = 1)
      H <-w0*zm %>% mean(trim=mtrim[i])
      betaa <- rbeta(1,H,1)
      eta_kt0[t] <- XB[t]+data.s0$ar$Estimate[1]*(link.kl$linkfun(yt0.matrix[t-1])-XB[t-1])+
        data.s0$ar$Estimate[2]*r[t-1]
      phit0 <- log(link.kl$linkinv(eta_kt0[t]))/log(1-0.5^(delta[t]))
      #bq_t=1/delta[t]
      #aq_t = 1/phit0
      yt0.matrix[t] <- (1-betaa^delta[t])^phit0
      #yt0.matrix[t] = (1-(1-0.5)^(1/(bq_t*H)))^(1/aq_t)
      #yt0.matrix[t] = (1-exp(-(delta[t]/w0)*(-log(1-.5))^(1/alfas)))^phit0
      r[t] <- link.kl$linkfun(yt0.matrix[t]) - eta_kt0[t]

    }
    yt0[,b] <- yt0.matrix
  }
  est <- t(apply(yt0, 1, quantile,c(al/2,.5,1-al/2),rm.na=T)) %>% data.frame()
  rh <- rbind(rh,est)

}
colnames(rh)=c("x1","x2","x3")
#dd <- data.aux$data.complete%>%data.frame()%>% filter(City %in% city[1:12])
estt = cbind(data.aux$data.complete,rh) %>% data.frame()
dates <- seq.Date(as.Date("2000/01/01"),as.Date("2008/12/31"),by = "month",sep="/") %>% rep(14)
fig1 = estt %>% mutate(dates=dates) %>% select(dates,t,RH,Group,City,x1,x2,x3)%>%
  #pivot_longer(cols = c(x1,x2,x3,RH)) %>%
  dplyr::group_split(Group) %>%
  purrr::map(
    ~ggplot(.) +
      geom_line(aes(dates,RH),size=.1)+
      geom_line(aes(dates,x2),size=.1,colour="red")+
      geom_ribbon(aes(x=dates,ymin=x1,ymax=x3),alpha = 0.30)+
      #geom_point(aes(t,value))+
      ylab("Relative humidity")+ #scale_y_continuous(labels=scales::percent)+
      scale_x_date(date_labels = "%b-%y",breaks = c(seq(as.Date("2000/02/1"), as.Date("2008/12/31"), by = "years")),limits = c(as.Date("2000/01/01"),as.Date("2008/12/31")))+
      #scale_x_discrete(labels = c(month.abb))+
      facet_grid(~City,scale="free")+
      theme_bw()+#geom_hline(data = d,aes(yintercept = kp))+
      #geom_hline(yintercept = q)+
      theme(legend.position = "",axis.text.x=element_text(angle=60,size=8,hjust = 1),
            axis.title.y = element_text(size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig1
aa <- estt %>% group_by(Group,City) %>% summarise(mse=mean(abs(RH-x2)/RH))
aa
mean(aa$mse)


