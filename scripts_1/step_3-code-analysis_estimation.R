# Generating the database/Analysis of results with estimates paramenters-------
rm(list=ls())
require(extraDistr)
require(tidyverse)
devtools::load_all()


path_estimation <- "estimations_predicts/estimations.rds"
results <- readRDS(path_estimation)

p <- results$p
q <- results$q
database <-results$data %>% arrange(Group)%>%group_by(City) %>% slice(-(1:(max(q,p))))
quantil <- results$quantil

cov_kl <- results$cov_kl
cov_delta <- results$cov_delta
ncx <- ncol(cov_kl)
ncv <- ncol(cov_delta)
link.kl <-results$link.kl$linkfun
link.eta <-results$link.kl$linkinv
alfa.est <- results$alpha[,1]

est <-  results$DADOS %>% data.frame() %>%
  mutate(bq_t=1/delta,
         phi_ts = log(kl_t)/log(1-(1-quantil)^(delta)),
         aq_t = 1/phi_ts,
         t=database$t,
         UR=y,
         Mês=database$month_names,
         Ano=database$Year,
         LONGITUDE=database$Longitude,
         LATITUDE=database$Latitude) %>%
  arrange(grupos)
head(est,13)

estt = NULL

n=1000
estt <-NULL
B=100

mtrim=c(.20,.20,.20)
  yest.aux <-cond.aux <-marg.aux<- matrix(0,nrow = nrow(est),ncol = B)
  for (B in 1:B){
    estt <-NULL
    for(j in 1:length(alfa.est)){




      #qest=NULL
      G <- paste("Group",j)
      alfa.aux <- alfa.est[j]
      mt<- mtrim[j]
      #set.seed(10)
      #u <- runif(100)
      e <- est %>% dplyr::filter(grupos==G) %>% mutate(zm = (rstable_pos(n,alfa.aux)) %>% mean(trim=mt),
                                                       H=(zm)*(weights),
                                                       qcond = 1-(1-quantil)^H,
                                                       qmarg = 1-link.eta(alfa.aux*(log(zm)+link.kl(1-qcond))),
                                                       bq_t=1/delta,
                                                       phi_ts = log(kl_t)/log(1-(1-quantil)^(delta)),
                                                       aq_t = 1/phi_ts,
                                                       Espy_z= bq_t*H*beta(1+1/aq_t,bq_t*H),
                                                       Vary_z= bq_t*H*beta(1+2/aq_t,bq_t*H)-(bq_t*H*beta(1+1/aq_t,bq_t*H))^2,
                                                       betaa=rbeta(1,H,1),
                                                       yest = (1-betaa^(delta))^(phi_ts),
                                                       ycond = (1-(1-runif(1))^(1/(bq_t*H)))^(1/aq_t),
                                                       ymarg = (1-exp(-(delta/(weights))*(-log(1-(runif(1))))^(1/alfa.aux)))^phi_ts,
                                                       Fy_z = pkumar(.7,aq_t,bq_t*zm*weights,lower.tail = F),
                                                       Fy = exp(-((-log(1-.7^(1/phi_ts))*weights)/delta)^alfa.aux),
                                                       r_qt = qnorm(pkumar(y,aq_t,bq_t*zm*weights,lower.tail = F)),
                                                       cofvar=sqrt(Vary_z)/Espy_z,
                                                       eta_1 = link.kl(1-exp(-(delta/(weights))*(-log(1-qmarg))^(1/alfa.aux))),
                                                       eta_2 = link.kl(1-(1-quantil)^(1/(bq_t*H))),
                                                       diF = abs(eta_1) - abs(eta_2),
                                                       klt=kl_t
      )


      estt=rbind(estt,e)
    }
    yest.aux[,B] <- estt$yest
    cond.aux[,B] <- estt$ycond
    marg.aux[,B] <- estt$ymarg
  }


#yest <- apply(yest.aux, 1,median)
estt <- estt %>% mutate(yest=apply(yest.aux, 1,quantile,c(.5)),
                        ycond=apply(cond.aux, 1,quantile,c(.5)),
                        ymarg=apply(marg.aux,1,mean,trim=.20)) %>% arrange(grupos)
head(estt,24)
est = estt %>% mutate(erro1=round(((UR-yest)/UR),2),
                      erro2=round(((UR-ycond)/UR),2),
                      erro3=round(((UR-ymarg)/UR),2))
estt$qcond %>% unique()
estt$qmarg%>% unique()
quantil
plot.ts(c(est$diF))
estt %>% group_by(grupos,idx)%>%summarise(erro1=mean((abs(UR-yest)/UR)),
                                          erro2=mean((abs(UR-ycond)/UR)),
                                          erro3=mean((abs(UR-ymarg)/UR)))

# Figuras para an?lise
dates <- seq.Date(as.Date("2000/02/01"),as.Date("2008/12/31"),by = "month",sep="/") %>% rep(14)
fig1 = estt %>% mutate(dates=dates) %>% select(dates,t,ymarg,UR,yest,grupos,idx,ymarg)%>%pivot_longer(cols = c(ymarg,UR)) %>%dplyr::group_split(grupos) %>%
  purrr::map(
    ~ggplot(.,aes(colour=name )) + geom_line(aes(dates,value),size=.1)+
      #geom_point(aes(t,value))+
      ylab("Relative humidity")+ #scale_y_continuous(labels=scales::percent)+
      scale_x_date(date_labels = "%b-%y",breaks = c(seq(as.Date("2000/1/1"), as.Date("2009/12/31"), by = "years")),limits = c(as.Date("2000/02/01"),as.Date("2009/12/31")))+
      #scale_x_discrete(labels = c(month.abb))+
      facet_grid(~idx,scale="free")+
      theme_bw()+#geom_hline(data = d,aes(yintercept = kp))+
      #geom_hline(yintercept = q)+
      theme(legend.position = "none",axis.text.x=element_text(angle=60,size=8,hjust = 1),
            axis.title.y = element_text(size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig1
cairo_ps("Figures/predict_value.eps")
fig1
dev.off()

#forecasting-------


## Loading database------

data("data_1")
head(data_1)
## Parse database------
month_names <- factor(month.abb, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
database <- data_1 %>%
  filter(Station == "1") %>%
  mutate(
    month_names = rep(month_names, 21) %>% rep(14),
    date = paste0(Year, "/", month_names),
    t = seq(1, 252) %>% rep(14),
    Group = rep(paste0("Group ", c(1, 2, 3, 3, 2, 1, 1, 3, 2, 3, 3, 3, 1, 1)), each = 252),
    cost1 = cos((2 * pi * as.numeric(Month)) / 12),
    sent1 = sin((2 * pi * as.numeric(Month)) / 12),
    cost2 = cos((2 * pi * as.numeric(Month)) / 6),
    sent2 = sin((2 * pi * as.numeric(Month)) / 6),
    cost3 = cos((2 * pi * as.numeric(Month)) / 4),
    sent3 = sin((2 * pi * as.numeric(Month)) / 4)
  )
head(database, 13)

y.mat <- matrix(database$RH,nrow = 252,ncol = 14)
colnames(y.mat) <- database$City %>% unique()
cor(y.mat)
## Calculating the weights ---------

u1 <- "Nova Olinda do Norte"
u2 <- "Maraã"
u3 <- "Itamarati"
u <- c(u1, u2, u3)

w <- list()
for (j in 1:3) {
  g <- paste0("Group ", j)
  dados_aux <- database %>% dplyr::filter(Group == g)
  u_lat_log <- dplyr::filter(data_1, City == u[j])[c("City", "Latitude", "Longitude")]
  lat_log <- unique(dados_aux[c("City", "Latitude", "Longitude")])
  aux <- rbind(lat_log, u_lat_log)
  w[j] <- weight(mun = aux$City, u_m = u[j], lat = as.numeric(aux$Latitude), long = as.numeric(aux$Longitude))
}
names(w) <- c(paste0("Group ", 1:length(u))) # u
w
sapply(w, sum)
# names(w)=u
w.data.frame <- tibble::enframe(w) %>%
  tidyr::unnest(cols = value) %>%
  select(value) %>%
  unlist() %>%
  rep(each = 252)
database <- database %>%
  arrange(Group) %>%
  mutate(weights = w.data.frame) %>%
  arrange(Group)
head(database)

TT <- 120 #
data <- database %>%
  group_by(City) %>%
  slice(1:TT)
city <- data$City %>% unique()
estimates <- NULL
for(city.choose in city){
city.choose <- city.choose
city <- results$DADOS %>% filter(idx==city.choose) %>% data.frame()
y <- data %>% filter(City==city.choose) %>% select(RH) %>% slice(c(TT-12+1):TT)
y <- c(0,y%>%pull(RH))
y.pred <- estt %>% filter(idx==city.choose) %>% select(yest)
kl.pred <- estt %>% filter(idx==city.choose) %>% select(klt)
betas <- results$kl[,1]
ar <-results$ar[1,1]
ma <-results$ar[2,1]
delta <- estt %>% select(delta) %>% slice(1:12)
delta <- c(0,delta$delta)
g = city %>%pull(grupos) %>% unique()
alfas <- results$alpha[as.numeric(str_sub(g,7)),1]
h=12
X.prev.dez <- city %>%  select(c(3:6)) %>% slice(107)
X.prev <- rbind(X.prev.dez,city %>%  select(c(3:6)) %>% slice(c(107-11):107))
XB.prev <- as.matrix(X.prev) %*%betas
#xb.p <- city %>%  select(c(3:10)) %>% slice(1:h)
#XB.prev <- rbind(0,XB.prev)
y.prev<-kl.prev<- vector(l=h+1)
y.prev[1] <- y.pred$yest[nrow(y.pred)]
kl.prev[1] <- kl.pred$klt[nrow(y.pred)]
weights <- estt %>% filter(idx==city.choose) %>% pull(weights)%>% unique()
mtrim = 0.19
r = rep(0,13)
B=100
y.prev.mat <- matrix(0,h+1,B)
for(b in 1:B){
  for(i in 2:(h+1)){
    kl.prev[i] <- link.eta(XB.prev[i]+ar*(link.kl(y.prev[i-1])-XB.prev[i-1]))+
      ma*r[i-1]
    #bq_t=1/delta[i,]
    phi_ts = log(kl.prev[i])/log(1-(1-quantil)^(delta[i]))
    zm <- (stabledist::rstable(1000,alpha = alfas,beta = 1,gamma = 1,delta = 0,pm = 1)) %>% mean(trim=mtrim)
    H=(zm)*(weights)
    betaa=rbeta(1,H,1)
    y.prev[i] <- ((1-betaa^(delta[i]))^(phi_ts[1]))
    r[i] <- link.kl(y.prev[i]) - link.kl(kl.prev[i])
  }
  y.prev.mat[,b] <- y.prev
}
al = 0.05
quant.est <- apply(y.prev.mat,1,quantile,c(al/2,0.5,1-al)) %>% t
aux <- data.frame(city=city.choose,quant.est,y=y,grupos=g)
estimates <- rbind(estimates,aux)
}
colnames(estimates) <- c("city","p1","p2","p3","y","grupos")
estimates %>% group_by(city) %>% slice(-1) %>% summarise(mape=mean((y-p2)/y))
dates <- seq.Date(as.Date("2009/01/01"),as.Date("2009/12/31"),by = "month",sep="/") %>% rep(14)
fig1 = estimates %>% group_by(city) %>% slice(-1) %>% data.frame %>% mutate(dates=dates) %>% dplyr::group_split(grupos) %>%
  purrr::map(
    ~ggplot(.) + geom_line(aes(dates, p2),size=.1,colour=2)+
      geom_line(aes(dates, y),size=.1,colour=1)+
      geom_ribbon(aes(x=dates,ymin=p1,ymax=p3),alpha = 0.30)+
      #geom_point(aes(t,value))+
      ylab("Relative humidity")+ #scale_y_continuous(labels=scales::percent)+
      scale_x_date(date_labels = "%b-%y",breaks = c(seq(as.Date("2009/01/01"), as.Date("2009/12/31"), by = "month")),limits = c(as.Date("2009/01/01"),as.Date("2009/12/31")))+
      #scale_x_discrete(labels = c(month.abb))+
      facet_grid(~city,scale="free")+
      theme_bw()+#geom_hline(data = d,aes(yintercept = kp))+
      #geom_hline(yintercept = q)+
      theme(legend.position = "none",axis.text.x=element_text(angle=60,size=8,hjust = 1),
            axis.title.y = element_text(size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig1


aux.est <- estimates %>% filter(city=="Manicoré")
plot.ts(aux.est[-1,5],col=1,type="o",ylim=c(0.5,.90))
lines(aux.est[-1,3],type="o",col=2)
lines(aux.est[-1,2],type="o",col=3)
lines(aux.est[-1,4],type="o",col=3)
mean((aux.est[-1,5]-aux.est[-1,3])/aux.est[-1,5])

yrh = data %>% filter(City==city.choose) %>% select(RH)
plot.ts(yrh$RH[c((TT-12+1):TT)],type="o")
plot.ts(yrh$RH,type="o")
abline(v=c((TT-12+1),TT))
abline(v=c(1,12))
x11()
fig1


saveRDS(estt,"estimations_predicts/predicted_values.rds")




fig1 = estt %>% mutate(dates=dates) %>% select(dates,t,ymarg,Fy_z,UR,yest,grupos,idx,ymarg)%>%pivot_longer(cols = c(Fy_z)) %>%dplyr::group_split(grupos) %>%
  purrr::map(
    ~ggplot(.) + geom_line(aes(dates,value),color="black",size=.3)+
      geom_point(aes(dates,value),size=0.1)+
      ylab(expression("Prob(RH>0.70)")) + # scale_y_continuous(labels=scales::percent)+
      scale_x_date(date_labels = "%b-%y",breaks = c(seq(as.Date("2000/1/1"), as.Date("2009/12/31"), by = "years")),limits = c(as.Date("2000/02/01"),as.Date("2009/12/31")))+
      #scale_x_date(date_labels = "%b-%y",date_breaks = "22 weeks",limits = c(as.Date("2000/02/01"),as.Date("2009/12/31")))+
      #scale_x_discrete(labels = c(month.abb))+
      facet_grid(~idx,scale="free")+
      theme_bw()+#geom_hline(data = d,aes(yintercept = kp))+
      geom_hline(aes(yintercept = 0.70),linetype =2)+
      theme(legend.position = "none",axis.text.x=element_text(angle=60,size=8,hjust = 1),
            axis.title.y = element_text(size=8),
            axis.text.y = element_text(size=8)))%>%cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig1
x11()
cairo_ps("Figures/predict_prob.eps")
fig1
dev.off()

fig2 = est %>%dplyr::group_split(grupos) %>%
  purrr::map(
    ~ggplot(.) + geom_boxplot(aes(Mês,r_qt))+
      ylab("Relative humidity")+ scale_y_continuous(labels=scales::percent)+
      scale_x_discrete(labels = c(month.abb))+
      facet_grid(~idx)+geom_hline(yintercept = 0)+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig2



fig3 = est %>%dplyr::group_split(grupos) %>%
  purrr::map(
    ~ggplot(.) + geom_point(aes(t,r_qt))+
      ylab("Residuals")+ scale_y_continuous()+
      geom_hline(yintercept = c(-3,3))+
      # scale_x_discrete(labels = c(month.abb))+
      facet_wrap(~idx,scale="free_y")+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig3




ic_alpha= function(alpha, acf_res){
  return(qnorm((1 + (1 - alpha))/2)/sqrt(acf_res$n.used))
}


acff <- est %>% group_by(idx,grupos) %>% summarise(acff=acf(r_qt,plot=F)$acf[,,1],
                                                   lag = acf(r_qt,plot=F)$lag,
                                                   lim1=ic_alpha(0.05,acf(r_qt)))



fig3 = acff %>% data.frame() %>%dplyr::group_split(grupos) %>%
  purrr::map(
    ~ggplot(.,aes(x=lag,y=acff)) + geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0))+
      ylab("ACF")+ scale_y_continuous()+
      geom_hline(aes(yintercept = lim1), linetype = 2, color = 'blue') +
      geom_hline(aes(yintercept = -lim1), linetype = 2, color = 'blue')+
      # scale_x_discrete(labels = c(month.abb))+
      facet_grid(~idx,scale="free_x")+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig3


ic_alpha= function(alpha, acf_res){
  return(qnorm((1 + (1 - alpha))/2)/sqrt(acf_res$n.used))
}


acff <- est %>% group_by(idx,grupos) %>% summarise(acff=pacf(r_qt,plot=F)$acf[,,1],
                                                   lag = pacf(r_qt,plot=F)$lag[,,1],
                                                   lim1=ic_alpha(0.05,pacf(r_qt)))



fig3 = acff %>% data.frame() %>%dplyr::group_split(grupos) %>%
  purrr::map(
    ~ggplot(.,aes(x=lag,y=acff)) + geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0))+
      ylab("PACF")+ scale_y_continuous()+
      geom_hline(aes(yintercept = lim1), linetype = 2, color = 'blue') +
      geom_hline(aes(yintercept = -lim1), linetype = 2, color = 'blue')+
      # scale_x_discrete(labels = c(month.abb))+
      facet_grid(~idx,scale="free_x")+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig3


acff <- database %>% group_by(City,Group) %>% summarise(acff=pacf(RH,plot=F)$pacf[,,1],
                                                        lag = pacf(RH,plot=F)$lag,
                                                        lim1=ic_alpha(0.05,pacf(RH)))



fig3 = acff %>% data.frame() %>%dplyr::group_split(Group) %>%
  purrr::map(
    ~ggplot(.,aes(x=lag,y=acff)) + geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0))+
      ylab("ACF")+ scale_y_continuous()+
      geom_hline(aes(yintercept = lim1), linetype = 2, color = 'blue') +
      geom_hline(aes(yintercept = -lim1), linetype = 2, color = 'blue')+
      # scale_x_discrete(labels = c(month.abb))+
      facet_grid(~City,scale="free_x")+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig3





fig1 = estt %>% select(t,q_est,ymarg,UR,y,grupos,idx,ymarg)%>%pivot_longer(cols = c(UR)) %>%dplyr::group_split(grupos) %>%
  purrr::map(
    ~ggplot(.,aes(colour=name )) + geom_line(aes(t,value))+
      #geom_point(aes(t,value))+
      ylab("Relative humidity")+ #scale_y_continuous(labels=scales::percent)+
      #scale_x_discrete(labels = c(month.abb))+
      facet_grid(~idx,scale="free")+
      theme_bw()+#geom_hline(data = d,aes(yintercept = kp))+
      #geom_hline(yintercept = q)+
      theme(axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig1



# Análise de Sensibilidade ------------------------------------------------


est <-  results$DADOS %>% data.frame() %>%
  mutate(bq_t=1/delta,
         phi_ts = log(kl_t)/log(1-(1-quantil)^(delta)),
         aq_t = 1/phi_ts,
         t=database$t,
         UR=y,
         Mês=database$month_names,
         Ano=database$Year,
         LONGITUDE=database$Longitude,
         LATITUDE=database$Latitude) %>%
  arrange(grupos)
head(est,13)

estt = NULL

mtrim <- seq(0.10,.30,by=0.01)
#mtrim <- c(.18,.22)
n=1000
estt <-NULL
B=20
mse <- matrix(0,nrow = nrow(est),ncol=length(mtrim))
for(k in 1:length(mtrim)){
  #mtrim=mtrim[k]
  yest.aux <-matrix(0,nrow = nrow(est),ncol = B)
  for (B in 1:B){
    estt <-NULL
    for(j in 1:length(alfa.est)){

      #qest=NULL
      G <- paste("Group",j)
      alfa.aux <- alfa.est[j]
      #set.seed(10)
      e <- est %>% dplyr::filter(grupos==G) %>% mutate(zm = rstable_pos(n,alfa.aux) %>% mean(trim=mtrim[k]),
                                                       H=zm*weights,
                                                       Espy_z= bq_t*H*beta(1+1/aq_t,bq_t*H),
                                                       Vary_z= bq_t*H*beta(1+2/aq_t,bq_t*H)-(bq_t*H*beta(1+1/aq_t,bq_t*H))^2,
                                                       betaa=rbeta(1,H,1),
                                                       yest = (1-betaa^(delta))^(phi_ts),
                                                       ycond = (1-(1-quantil)^(1/(bq_t*H)))^(1/aq_t),
                                                       ymarg = (1-exp(-(delta/weights)*(-log(1-quantil))^(1/alfa.aux)))^phi_ts,
                                                       Fy_z = pkumar(.7,aq_t,bq_t*zm*weights,lower.tail = F),
                                                       r_qt = qnorm(pkumar(y,aq_t,bq_t*zm*weights,lower.tail = F)),
                                                       cofvar=sqrt(Vary_z)/Espy_z,
      )


      estt=rbind(estt,e)
    }
    yest.aux[,B] <- estt$yest


  }
  yest <- apply(yest.aux, 1,median)
  mse[,k] <- (100*(yest-estt$UR)/estt$UR)

}
colnames(mse) <- c(paste0("m_",mtrim))
estt <- estt %>% mutate(yest=apply(yest.aux, 1,median)) %>% arrange(grupos)
head(estt,24)
est = estt %>% mutate(erro1=round(((UR-yest)/UR),2),erro2=scale(UR-yest))
est
teste <- cbind(est,mse)
colMeans(mse)
cbind(teste %>% group_by(grupos) %>% summarise(m=mean(m_0.19)),
      teste %>% group_by(grupos) %>% summarise(m=mean(m_0.2)),
      teste %>% group_by(grupos) %>% summarise(m=mean(m_0.21)))
weighted.mean(c(0.19,0.20,.21),w = c(2,1,1))

