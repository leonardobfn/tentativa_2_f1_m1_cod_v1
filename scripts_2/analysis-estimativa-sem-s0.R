# Generating the database/Analysis of results with estimates paramenters-------
rm(list=ls())
require(extraDistr)
require(tidyverse)
devtools::load_all()

data.aux <- readRDS("scripts_2/1-estimations_Barcelos.rds")
city <- data.aux$data.complete %>% group_by(Group, City) %>% pull(City) %>% unique
results.full <- NULL
i = 10
path_estimation <- paste0("scripts_2/", i, "-estimations_", city[i], ".rds")


results <- readRDS(path_estimation)

p <- results$p
q <- results$q
database <-results$data %>% arrange(Group)%>%group_by(City) %>% slice(-(1:(max(q,p))))
quantil <- results$quantil

cov_kl <- results$cov_kl
cov_delta <- results$cov_delta
ncx <- ncol(cov_kl)
ncv <- ncol(cov_delta)

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

mtrim=0.20
yest.aux <-matrix(0,nrow = nrow(est),ncol = B)
for (B in 1:B){
  estt <-NULL
  for(j in 1:length(alfa.est)){

    #qest=NULL
    G <- paste("Group",j)
    alfa.aux <- alfa.est[j]
    #set.seed(10)
    e <- est %>% dplyr::filter(grupos==G) %>% mutate(zm = rstable_pos(n,alfa.aux) %>% mean(trim=mtrim),
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
  yest.aux[,B] <- estt$ymarg

}
#yest <- apply(yest.aux, 1,median)
estt <- estt %>% mutate(yest=apply(yest.aux, 1,median)) %>% arrange(grupos)
head(estt,24)
est = estt %>% mutate(erro1=round(((UR-yest)/UR),2),erro2=scale(UR-yest))
est

# Figuras para an?lise
dates <- seq.Date(as.Date("2000/02/01"),as.Date("2009/12/31"),by = "month",sep="/") %>% rep(13)
fig1 = estt %>% mutate(dates=dates) %>% select(dates,t,ymarg,UR,yest,grupos,idx,ymarg)%>%pivot_longer(cols = c(yest,UR)) %>%dplyr::group_split(grupos) %>%
  purrr::map(
    ~ggplot(.,aes(colour=name )) + geom_line(aes(dates,value),size=.1)+
      #geom_point(aes(t,value))+
      ylab("Relative humidity")+ #scale_y_continuous(labels=scales::percent)+
      scale_x_date(date_labels = "%b-%y",breaks = c(seq(as.Date("2000/1/1"), as.Date("2009/12/31"), by = "years")),limits = c(as.Date("2000/02/01"),as.Date("2009/12/31")))+
      #scale_x_discrete(labels = c(month.abb))+
      facet_grid(~idx,scale="free")+
      theme_bw()+#geom_hline(data = d,aes(yintercept = kp))+
      #geom_hline(yintercept = q)+
      theme(axis.text.x=element_text(angle=60,size=8,hjust = 1),
            axis.title.y = element_text(size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
  x11()
  fig1
cairo_ps("Figures/predict_value.eps")
fig1
dev.off()

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
B=100
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

