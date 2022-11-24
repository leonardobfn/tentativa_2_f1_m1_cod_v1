
rm(list = ls())

# Packages -------
pack <-
  c("tidyverse",
    "extraDistr",
    "devtools",
    "Formula",
    "tictoc",
    "betareg",
    "cowplot")

package.check <- lapply(
  pack,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

devtools::load_all() # loading my functions

# Estimativa para yt(s0) -----------------------------------------------
data.aux <- readRDS("scripts_2/2-estimations_Fonte Boa.rds")
city <- data.aux$data.complete %>% group_by(Group, City) %>% pull(City) %>% unique
results.full <- sl<-  NULL
N=1000
u <- matrix(0,N,108)
for(i in 1:14){

  path <- paste0("scripts_2/", i, "-estimations_", city[i], ".rds")
  results <- readRDS(path)
  s0 <- results$s0
  g <-results$data.complete %>% filter(City == s0) %>% pull(Group) %>% unique()
  y.s0 <- results$data.complete %>% filter(City == s0)
  y.sl.g <- results$data.complete %>% filter(Group==g,City != s0)

  city.grupos <- unique(y.sl.g$City)
  delta <- results$DADOS %>% filter(idx==city.grupos[1]) %>%  pull(delta)
  alfas <- results$alpha$Estimate[as.numeric(str_sub(g,7))]
  min.max <- y.sl.g %>% group_by(Year,Month) %>% summarise(min=min(RH),max=max(RH),
                                                           d=max-min)
  #y.sl.g %>% filter(Year=="2000",Month=="02")
  a.b <- y.sl.g %>% group_by(Year,Month) %>% summarise(a=mean(RH)-sd(RH)*sqrt(3),b=mean(RH)+sd(RH)*sqrt(3))
  start <- y.sl.g %>% filter(Year=="2000" & Month=="01") %>% pull(RH) %>% mean




  RH <- y.sl.g %>% filter(City%in%city.grupos)%>%group_by(City)%>% slice(-1) %>% pull(RH) %>% data.frame()
  #a.b <- c(mean(RH$.)-sd(RH$.)*sqrt(3),mean(RH$.)+sd(RH$.)*sqrt(3))
  min.max <- c(max(y.sl.g %>% pull(RH) )-min(y.sl.g %>% pull(RH) ))
  mean.grupo <- mean(RH$.)
  dates <- results$data.complete %>%
    filter(City%in%city.grupos) %>%
    group_by(City)%>%
    slice(-1) %>%
    select(Year,Month) %>%
    data.frame()
  w0 <- y.s0  %>% pull(weights.w0) %>% unique()
  XB <- model.matrix(Formula::Formula(results$formula),y.s0,rhs=1)%*%
    results$kl$Estimate
  link.kl <- results$link.kl
  s1 <- results$DADOS %>% filter(idx%in%city.grupos) %>% select(weights,delta,kl_t) %>% data.frame() %>%
    mutate(delta=delta[,1],
           RH=RH$.,
           phi=log(kl_t)/log(1-0.5^(delta)),
           ytil=-log(1-RH^(1/phi)),
           x=ytil*weights/delta,
           Year=dates$Year,
           Month=dates$Month) %>%
    group_by(Year,Month) %>%
    summarise(s1=sum(x)) %>% pull(s1)
  s1<-c(0,s1)
  delta <- c(0,delta)
  B=20
  yt0 <- matrix(0,nrow = 108,B)
  yt0.aux <- eta_kt0 <-  rep(NA,length = 108)
  yt0.aux[1] <-link.kl$linkinv(XB[1])#y.s0$RH[1]
   quantil <- results$quantil
   zm <- stabledist::rstable(1000,alpha = alfas,beta = 1,gamma = 1,delta = 0,pm = 1) %>% mean(trim=0.22)
   alfas <- link.kl$linkfun(1-quantil)/(log(zm)+link.kl$linkfun(1-quantil))
   #quantil.m <- 1-link.kl$linkinv((log(zm)*alfas)/(alfas-1))
  r <- rep(0,108)
  for(j in 1:B){


    for(t in 2:108){

      #alfas <- link.kl$linkfun(1-quantil)/(log(zm)+link.kl$linkfun(1-quantil))

      #u <- runif(N,a.b$a[t],a.b$b[t])# não deu certo
      #u <- runif(N,a.b[1],a.b[2])# não deu certo
      #u[,t] <- runif(N,.35,.50)
      u <- runif(1,0,1)
      #u <- runif(N,min.max[1],min.max[2])
      # k <- sample(1:6,size = 6,replace = T,prob=NULL)
      # k <- k/sum(k)
      # u[,t] <-  k[1]*runif(N,0,.10)+k[2]*runif(N,.10,.20)+k[3]*runif(N,.20,.30)+
      #   k[4]*runif(N,.30,.40)+k[5]*runif(N,.40,.50)+k[6]*runif(N,.50,.60)#+
      #   #k[7]*runif(N,.60,.70)#+k[8]*runif(N,.70,.80)#+k[9]*runif(N,.80,.90)#+
        #k[10]*runif(N,.90,1)

      eta_kt0[t] <- XB[t]#+results$ar$Estimate[1]*(link.kl$linkfun(yt0.aux[t-1])-XB[t-1])+
     # results$ar$Estimate[2]*r[t-1]
      phi0 <- log(link.kl$linkinv(eta_kt0[t]))/log(1-(1-(quantil))^(delta[t]))
      ytil0 <- (((s1[t])^alfas-log(1-u))^(1/alfas)-s1[t])*delta[t]/w0
      yt0.aux[t]<- mean((1-exp(-ytil0))^(phi0),trim=0)
      #yt0.aux[t] = mean((1-exp(-(delta[t]/(w0))*(-log(1-(u)))^(1/alfas)))^phi0)

      r[t] <- link.kl$linkfun(yt0.aux[t]) - eta_kt0[t]
    }
    yt0[,j] <- yt0.aux
  }
  est <- apply(yt0,1,mean,trim=.5,na.rm=T)
  sl.aux <- data.frame(City=s0,RH_est=est,w=w0)
  sl <- rbind(sl,sl.aux)
}

dates <- seq.Date(as.Date("2000/01/01"),as.Date("2008/12/31"),by = "month",sep="/") %>% rep(14)
estt = data.aux$data.complete%>%data.frame()%>%mutate(RH_est=sl$RH_est)
#estt = data.aux$data.complete%>%data.frame()%>% filter(City %in% city[1:10])%>%mutate(RH_est=sl$RH_est)
fig1 = estt %>% mutate(dates=dates) %>% select(RH_est,dates,t,RH,Group,City)%>%
  pivot_longer(cols = c(RH_est,RH)) %>%dplyr::group_split(Group) %>%
  purrr::map(
    ~ggplot(.,aes(colour=name )) + geom_line(aes(dates,value),size=.1)+
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

w <-  unique.data.frame(estt %>% select(City,weights.w0))
estt %>% group_by(Group,City) %>% summarise(mse=mean(abs(RH_est-RH)/RH,na.rm=T)) %>% data.frame() %>% mutate(w=w$weights.w0)
