require(extraDistr)
require(tidyverse)
rm(list=ls())
path_estimation <- "estimations_predicts/estimations.rds"
results <- readRDS(path_estimation)

p <- results$p
q <- results$q
database <-results$data %>% arrange(Group)%>%group_by(City) %>% slice(-(1:(max(q,p))))
quantil <- results$quantil

EMVteta2 <- c(results$ar$Estimate,
              results$kl$Estimate,
              results$delta$Estimate,
              results$alpha$Estimate)

cov_kl <- results$cov_kl
cov_delta <- results$cov_delta
ncx <- ncol(cov_kl)
ncv <- ncol(cov_delta)
TT <- results$TT #- max(results$p,results$q)
#betas <- EMVteta2[-(1:(p+q))][(1:ncx)]
#lambda <- EMVteta2[-(1:(p+q))][(ncx+1):(ncx+ncv)]
#phi_ar.am <- EMVteta2[(1:(p+q))]
alfa.est <- EMVteta2[-(1:(p+q))][(ncx+ncv+1):length(EMVteta2[-(1:(p+q))])]

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

rstable_pos <- function(n,alfa){
  U <- runif(n,0,pi)
  v <- rexp(n,1)
  a_U <- ((sin(alfa*U)/sin(U))^(1/(1-alfa)))*((sin((1-alfa)*U))/sin(alfa*U))
  E <- (a_U/v)^((1-alfa)/alfa)
  return(E)

}

n=1000
estt <-NULL
B=100
mtrim=.21
ycond.aux <-matrix(0,nrow = nrow(est),ncol = B)
for (B in 1:B){
  estt <-NULL
  for(j in 1:length(alfa.est)){

    #qest=NULL
    G <- paste("Group",j)
    alfa.aux <- alfa.est[j]
    #set.seed(10)
    e <- est %>% dplyr::filter(grupos==G) %>% mutate(zm = rstable_pos(1000,alfa.aux) %>% mean(trim=mtrim),
                                                     H=zm*weights,
                                                     Espy_z= bq_t*H*beta(1+1/aq_t,bq_t*H),
                                                     Vary_z= bq_t*H*beta(1+2/aq_t,bq_t*H)-(bq_t*H*beta(1+1/aq_t,bq_t*H))^2,
                                                     betaa=rbeta(1,H,1),
                                                     yest = (1-betaa^(delta))^(phi_ts),
                                                     ycond = (1-(1-runif(1))^(1/(bq_t*H)))^(1/aq_t),
                                                     ymarg = (1-exp(-(delta/weights)*(-log(1-runif(1)))^(1/alfa.aux)))^phi_ts,
                                                     Fy_z = pkumar(.7,aq_t,bq_t*zm*weights,lower.tail = F),
                                                     r_qt = qnorm(pkumar(y,aq_t,bq_t*zm*weights,lower.tail = F)),
                                                     cofvar=sqrt(Vary_z)/Espy_z,
    )


    estt=rbind(estt,e)
  }
  #ycond.aux[,B] <- estt$ycond
  ycond.aux[,B] <- estt$yest

}
head(estt,3)
al = 0.05
qest <- apply(ycond.aux,1,quantile,prob=c(al/2,0.5,1-al/2))%>%t()
#qest <- apply(ycond.aux,1,quantile,prob=c(0.05,0.5,0.95))%>%t()
colnames(qest) <- c("q25","q50","q75")
dates <- seq.Date(as.Date("2000/02/01"),as.Date("2008/12/31"),by = "month",sep="/") %>% rep(14)
resul.env <- data.frame(Estações=est$idx,
                        Grupos=est$grupos,
                        UR=est$UR,
                        Ano=est$Ano,
                        Mês=est$Mês,
                        t=est$t,
                        qest) %>% mutate(d=ifelse(UR>=q25 & UR<=q75,1,0)) %>%
  mutate(dates = dates)
head(resul.env)
resul.env %>% group_by(Estações) %>% summarise(in.side=sum(d)/(TT))
xtable::xtable(resul.env %>% group_by(Estações) %>% summarise(in.side=sum(d)/(TT)),align=c("c","c","c"))

fig2 = resul.env %>%dplyr::group_split(Grupos) %>%
  purrr::map(
    ~ggplot(.)+geom_line(aes(x=dates,y=UR))+#geom_point(aes(x=t,y=UR,colour=factor(d)))+
      #geom_line(aes(x=t,y=q50),col=2)+
      #geom_line(aes(x=t,y=q50,colour="red"))+
      geom_ribbon(aes(x=dates,ymin=q25,ymax=q75),alpha = 0.30)+
      ylab("Relative humidity")+ #scale_y_continuous(labels=scales::percent)+
      scale_x_date(date_labels = "%b-%y",breaks = c(seq(as.Date("2000/1/1"), as.Date("2008/12/31"), by = "years")),limits = c(as.Date("2000/02/01"),as.Date("2008/12/31")))+
      #scale_x_date(date_labels = "%Y %b",date_breaks = "12 months",limits = c(as.Date("2000/01/01"),as.Date("2009/12/01")))+
      facet_grid(~Estações,scale="free")+
      theme_bw()+
      #geom_hline(data = d,aes(yintercept = kp))+
      #geom_hline(yintercept = q)+
      theme(legend.position = "none",axis.text.x=element_text(angle=60,size=10,hjust = 1),
            axis.title.y = element_text(size=10)),
    )%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
#cairo_ps("Figures/envelop.eps")
fig2
dev.off()

resul.env.g <- resul.env %>%filter(Grupos=="Group 1")

f = resul.env.g %>%
ggplot(.)+geom_line(aes(x=t,y=UR))+#geom_point(aes(x=t,y=UR,colour=factor(d)))+
  #geom_line(aes(x=t,y=q50),col=2)+
  #geom_line(aes(x=t,y=q50,colour="red"))+
  geom_ribbon(aes(x=t,ymin=q25,ymax=q75),alpha = 0.5)+
  ylab("Relative humidity")+ #scale_y_continuous(labels=scales::percent)+
  #scale_x_discrete(labels = c(month.abb))+
  facet_wrap(~Estações,scale="free",ncol=2)+
  theme_bw()+
  #geom_hline(data = d,aes(yintercept = kp))+
  #geom_hline(yintercept = q)+
  theme(legend.position = "none",axis.text.x=element_text(angle=90,size=8))
x11()
f


fig2 = resul.env %>%dplyr::group_split(Grupos) %>%
  purrr::map(
    ~ggplot(.)+geom_line(aes(x=t,y=UR))+
      geom_line(aes(x=t,y=q50,colour="red"))+#geom_point(aes(x=t,y=UR,colour=factor(d)))+
      #geom_line(aes(x=t,y=q50),col=2)+
      #geom_line(aes(x=t,y=q50,colour="red"))+
      #geom_ribbon(aes(x=t,ymin=q25,ymax=q75),alpha = 0.5)+
      ylab("Relative humidity")+ #scale_y_continuous(labels=scales::percent)+
      #scale_x_discrete(labels = c(month.abb))+
      facet_grid(~Estações,scale="free")+
      theme_bw()+
      #geom_hline(data = d,aes(yintercept = kp))+
      #geom_hline(yintercept = q)+
      theme(legend.position = "none",axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig2
