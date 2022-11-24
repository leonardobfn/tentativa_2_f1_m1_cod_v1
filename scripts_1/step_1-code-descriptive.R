rm(list=ls())

# Packages -------
pack <- c("tidyverse","extraDistr","devtools","Formula","tictoc","betareg","cowplot")

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

# Database------

## Loading database------

data("data_1")
head(data_1)
## Parse database------
month_names <- factor(month.abb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
database <- data_1 %>% filter(Station=="1") %>%
  mutate(month_names = rep(month_names,21) %>% rep(14),
         data=paste0(Year,"/",month_names),
         t =seq(1,252) %>% rep(14),
         Group = rep(paste0("Group ",c(1,2,3,3,2,1,1,3,2,3,3,3,1,1)),each=252),
         Latitude  = scale( Latitude %>%as.numeric()),
         Longitude  = scale(Longitude %>%as.numeric()),
         cost = cos(2*pi*as.numeric(Month)/12),
         sent=sin(2*pi*as.numeric(Month)/12),
         sem = rep(c(1,2),each=6) %>% rep(21) %>% rep(14),
         quart = rep(c(1,2,3),each=4) %>% rep(21) %>% rep(14))

head(database)


TT=120
database <-database %>% group_by(Group,City) %>% slice(1:TT) %>% data.frame()
fig1 = database %>%dplyr::group_split(Group) %>%
  purrr::map(
    ~ggplot(.) + geom_line(aes(t,RH))+geom_point(aes(t,RH))+
      #ylab("Relative humidity")+ scale_y_continuous(labels=scales::percent)+
      #scale_x_discrete(labels = c(month.abb))+
      facet_grid(~City)+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig1




fig1 = database %>%dplyr::group_split(Group) %>%
  purrr::map(
    ~ggplot(.) + geom_histogram(aes(RH))+
      #ylab("Relative humidity")+ scale_y_continuous(labels=scales::percent)+
      #scale_x_discrete(labels = c(month.abb))+
      facet_grid(~City)+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig1







fig1 = database %>%dplyr::group_split(Grupos) %>%
  purrr::map(
    ~ggplot(.) + geom_boxplot(aes(Mês,UR))+
      ylab("Relative humidity")+ scale_y_continuous(labels=scales::percent)+
      scale_x_discrete(labels = c(month.abb))+
      facet_grid(~Esta??es)+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,size=8)))%>%
  cowplot::plot_grid(plotlist = .,nrow=3)
x11()
fig1



fig1 = ggplot(database) + geom_boxplot(aes(M?s,UR))+
      ylab("Relative humidity")+ scale_y_continuous(labels=scales::percent)+
      scale_x_discrete(labels = c(month.abb))+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,size=8))
x11()
fig1


database %>%  group_by(City,sem) %>% summarise(m=mean(RH),s = sd(RH)) %>% data.frame()
database %>%  group_by(City,quart) %>% summarise(m=mean(RH),s = sd(RH)) %>% data.frame()
database %>% filter(City=="Coari") %>% group_by(sem) %>% summarise(s=mean(RH))


fig1 = ggplot(database) + geom_point(aes(t,UR))+geom_line(aes(t,UR))+
  ylab("Relative humidity")+ scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,size=8))
x11()
fig1

a = database %>% group_by(City,Mês) %>% slice(1:TT) %>% summarise(qq = quantile(RH,c(0.25,0.5,0.75))) %>% data.frame()
plot(a$qq~a$City)




require(quantreg)

erq <- RH~(Altitude) + sent+ cost
p.erq <- rq(erq,tau=c(.25,.5,.75),data=database)
plot(p.erq)
s.erq <- summary(p.erq)

summary(p.erq,se="boot",R=10)


erq1  <- rq(erq,tau = 0.10, method="br", data=database)
erq25 <- rq(erq,tau = 0.25, method="br", data=database)
erq5  <- rq(erq,tau = 0.50, method="br", data=database)
erq75 <- rq(erq,tau = 0.75, method="br", data=database)
erq9  <- rq(erq,tau = 0.90, method="br", data=database)
#--------------------------------------------------------------------------------------------------------------
#Anova function for quantile regression fits
#Por coeficientes
anovapeae <- anova.rq(erq1,erq5,erq9, test = "Wald", joint=FALSE)
#vizualizando o resultado
anovapeae
#Salvando os resultados em arquivo .csv
capture.output(anovapeae,file="anovapeae.csv")
#Testando conjuntamente a regressão
anovapeaej <- anova.rq(erq1,erq5,erq9, test = "Wald", joint=TRUE)
#vizualizando o resultado
anovapeaej
#salvando os resultados em arquivo txt
capture.output(anovapeaej,file="anovapeaej.txt")



#Gráfico de coeficientes quantílicos
#In order to obtain a more meaningful plot, we use a larger set of T
taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)
e.erq <- rq(erq,tau = taus, data=database)
qe.erq <- summary(e.erq, se="iid")
plot(qe.erq)

