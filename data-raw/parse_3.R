require(tidyverse)
rm(list=ls())

#UR_in = read.table("G:/Meu Drive/UFMG - Doutorado/Tese/TesePacoteR/dados_H/Ur.txt")
path_ur_input <- "data-raw/weather_stations/ur_input.txt"
ur_input = read.table(path_ur_input)
#tbs_in = read.table("G:/Meu Drive/UFMG - Doutorado/Tese/TesePacoteR/dados_H/tbs.txt")
#tbu_in = read.table("G:/Meu Drive/UFMG - Doutorado/Tese/TesePacoteR/dados_H/tbu.txt")
#precp_in = read.table("G:/Meu Drive/UFMG - Doutorado/Tese/TesePacoteR/dados_H/precep.txt")

path_data_raw_1 <- "data-raw/weather_stations/data_raw_1.RDS"
dados_brutos <- readRDS(path_data_raw_1)

dados_brutos[ur_input[,1],"UR"] = ur_input[,2]
# dados_brutos[tbs_in[,1],"TBS"] = tbs_in[,2]
# dados_brutos[tbu_in[,1],"TBU"] = tbu_in[,2]
# dados_brutos[precp_in[,1],"PrecpTotal"] = precp_in[,2]
head(dados_brutos)

dados_brutos %>% group_by(Estações) %>% summarise(n=length(Estações),
                                                  nn=length(which(is.na(UR)==T)),
                                                  p=nn/n)
 extremo <- function(x){
   return((max(x)+min(x))*.5)
 }
# d <- dados_brutos %>% group_by(Estações,Ano,Mês,Dia) %>% summarise(UR=extremo(UR)) %>%
#   group_by(Estações,Ano,Mês) %>% summarise(UR=min(UR))
#
# d$t <- rep(seq(1,252),14)
# d$Grupos = rep(paste0("Grupo ",c(1,2,3,3,2,1,1,3,2,3,3,3,1,1)),each=252)
# b = d %>% filter(Estações=="Coari")
# b %>% data.frame()
# x11()
# plot.ts(b$UR[200:252])
# b[205:252,] %>% data.frame()
# fig2 = d %>% data.frame() %>%dplyr::group_split(Grupos) %>%
#   purrr::map(
#     ~ggplot(.) + geom_point(aes(t,UR))+geom_line(aes(t,UR))+
#       ylab("Relative humidity")+ #scale_y_continuous(labels=scales::percent)+
#       #scale_x_discrete(labels = c(month.abb))+
#       facet_grid(~Estações,scale="free_y")+
#       theme_bw()+
#       #geom_hline(yintercept = q)+
#       theme(axis.text.x=element_text(angle=90,size=8)))%>%
#   cowplot::plot_grid(plotlist = .,nrow=3)
# x11()
# fig2
#
#



# dados = dados %>% select(everything())%>%
#   group_by(Estações,Ano,Mês,Dia)%>%summarise(Ex = (max(UR)+min(UR))/2)%>%
#   group_by(Estações,Ano,Mês)%>%summarise(Extremo_mes = median(Ex)) %>% data.frame()

length_est = tapply(dados_brutos$Estações,dados_brutos$Estações,length)[1]

dados_brutos = dados_brutos %>% mutate(Grupos = rep(paste0("Group ",c(1,2,3,3,2,1,1,3,2,3,3,3,1,1)),each=length_est),
                         Altitude = rep(c(30.65,78.41,34.18,30.31,123.42,59.34,107.73,19.6,62.23,48.86,40.01,29,79.71,56),each=length_est),
                         Estação=rep(1,14*length_est))
head(dados_brutos)
#  Colocando os valores das altitudes ------

require(rgdal)
path_map_am <- "map_AM/13mu2500gc.shp"
am<-readOGR(path_map_am)
am@data[,2] = iconv(am@data[,2], "UTF-8","latin1")
am@data[10,2] = "SGC"

Estações = c("Barcelos",
             "Benjamin Constant",
             "Coari",
             "Codajás",
             "Eirunepé",
             "Fonte Boa",
             "Iaurete",
             "Itacoatiara",
             "Lábrea",
             "Manaus",
             "Manicoré",
             "Parintins",
             "SGC",
             "Tefé")



am.dados.est = am@data %>% dplyr::filter(NOME %in% Estações) %>%
  rbind(c(1,"Iauarete","AM","13","Norte","NORTE AMAZONENSE","RIO NEGRO",0.61,-69.18,"T"))%>%
  arrange(NOME) %>%
  dplyr::select(LATITUDE,LONGITUDE,MESOREGIAO,MICROREGIA) %>%
  apply(MARGIN = 2,FUN = rep,each=length_est) %>% data.frame()


data.full = cbind(dados_brutos,am.dados.est)
head(data.full)
saveRDS(data.full,"data-raw/data.full.rds")

# Salvando subset de dados ---------

## data_1  ---------

for(i in 1:1){
dados <- data.full %>%
  group_by(Estações,Altitude,LATITUDE,LONGITUDE,Ano,Mês,Dia) %>%
  summarise(Ex_ur = extremo(UR),
            tbs = extremo(TBS),
            tbu = extremo(TBU),
            precp=sum(PrecpTotal))%>%
  group_by(Estações,Altitude,LATITUDE,LONGITUDE,Ano,Mês)%>%
  summarise(UR = min(sample(Ex_ur,30,prob=NULL,replace=T)),
            TBS=min(tbs),
            TBU=min(tbu),
            precp=sum(precp)) %>%
  select(Estações,Ano,Mês,UR,TBS,TBU,precp,Altitude,LATITUDE,LONGITUDE)
dados
am.dados.est = am@data %>% dplyr::filter(NOME %in% Estações) %>%
  rbind(c(1,"Iauarete","AM","13","Norte","NORTE AMAZONENSE","RIO NEGRO",0.61,-69.18,"T"))%>%
  arrange(NOME) %>%
  dplyr::select(LATITUDE,LONGITUDE,MESOREGIAO,MICROREGIA) %>%
  apply(MARGIN = 2,FUN = rep,each=nrow(dados)/14) %>%
  cbind(dados %>% data.frame()%>% select(-LATITUDE,-LONGITUDE)) %>%
  mutate(Estação=rep(1,nrow(dados)))%>%
  select(Estações,Ano,Mês,UR,TBS,TBU,precp,Estação,everything())

dados$Estação <-am.dados.est$Estação

am.dados.mun = am@data %>% dplyr::filter(NOME %in% Estações==F) %>%
  cbind(data.frame(matrix(NA,49,ncol(dados))))%>%
  arrange(NOME)%>%
  dplyr::select(NOME,X1,X2,X3,X4,X5,X6,X7,LATITUDE,LONGITUDE,X8)%>%
  mutate(X8=rep(0,49))



colnames(am.dados.mun)=colnames(dados)
dados = rbind(dados %>% data.frame(),am.dados.mun) %>% rename(City=Estações,Year=Ano,Month=Mês,Station=Estação,RH=UR,
                                                            Latitude=LATITUDE,Longitude=LONGITUDE) %>% select(-TBS,-TBU,-precp)
#head(dados)
data_1 <- dados
path <- paste0("data_bootrstap/","data_",i,"_boostrap.rds")

#saveRDS(dados,path)
#usethis::use_data(data_1,overwrite = T)
}
head(dados)



## data_2  ---------

dados <- data.full %>%
  # group_by(Estações,Altitude,LATITUDE,LONGITUDE,Ano,Mês,Dia) %>%
  # summarise(Ex_ur = extremo(UR),
  #           tbs = extremo(TBS),
  #           tbu = extremo(TBU),
  #           precp=sum(PrecpTotal))%>%
  group_by(Estações,Altitude,LATITUDE,LONGITUDE,Ano,Mês)%>%
  summarise(UR = mean(UR),
            TBS=min(TBS),
            TBU=min(TBU),
            precp=sum(PrecpTotal)) %>%
  select(Estações,Ano,Mês,UR,TBS,TBU,precp,Altitude,LATITUDE,LONGITUDE)
dados
am.dados.est = am@data %>% dplyr::filter(NOME %in% Estações) %>%
  rbind(c(1,"Iauarete","AM","13","Norte","NORTE AMAZONENSE","RIO NEGRO",0.61,-69.18,"T"))%>%
  arrange(NOME) %>%
  dplyr::select(LATITUDE,LONGITUDE,MESOREGIAO,MICROREGIA) %>%
  apply(MARGIN = 2,FUN = rep,each=nrow(dados)/14) %>%
  cbind(dados %>% data.frame()%>% select(-LATITUDE,-LONGITUDE)) %>%
  mutate(Esta??o=rep(1,nrow(dados)))%>%
  select(Estações,Ano,Mês,UR,TBS,TBU,precp,Esta??o,everything())

dados$Esta??o <-am.dados.est$Esta??o

am.dados.mun = am@data %>% dplyr::filter(NOME %in% Estações==F) %>%
  cbind(data.frame(matrix(NA,49,ncol(dados))))%>%
  arrange(NOME)%>%
  dplyr::select(NOME,X1,X2,X3,X4,X5,X6,X7,LATITUDE,LONGITUDE,X8)%>%
  mutate(X8=rep(0,49))



colnames(am.dados.mun)=colnames(dados)
dados = rbind(dados %>% data.frame(),am.dados.mun)
head(dados)
data_2 <- dados
usethis::use_data(data_2,overwrite = T)

data(data_1)
