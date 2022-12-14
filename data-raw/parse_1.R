rm(list = ls())

#------- 1-Packages---------
require(dplyr)
require(ggplot2)
require(extraDistr)
#require(plyr)

#--- 2-loading the raw data -----------
path_weather_stations = paste0("data-raw/weather_stations")
file_list =  list.files(path=weather_stations,pattern = ".csv")
dados.brutos= n = NULL
for(j in 1:length(file_list)){

  file = paste0(getwd(),"/",file_list[j])
  aux = read.csv(file,sep = ";",dec = ".",
                 encoding ="LATIN 1",na.strings = "null",col.names = c("Datas","Hora","NA","NB","NM","NEB","PrecpTotal","PressAtmEst","PressAtmMar",
                                                                       "TempBulboSeco","TempBulboUmido","TempOrvalho","UR","VentoDir","VentoVelo",
                                                                       "Visibilidade"))
  dados.brutos = rbind(dados.brutos,aux)
  n[j] = nrow(aux)
  rm(aux)
}

head(dados.brutos)
Esta??es = c("Barcelos",
             "Benjamin Constant",
             "Coari",
             "Codaj?s",
             "Eirunep?",
             "Fonte Boa",
             "Iaurete",
             "Itacoatiara",
             "L?brea",
             "Manaus",
             "Manicor?",
             "Parintins",
             "SGC",
             "Tef?")

#----- 3- Organizando dados brutos -----

L=length(Esta??es)
Data = H =NULL
for(j in 1:length(n)){
  Data.aux = data.frame(Data = seq.Date(as.Date("2000/01/01"),by = "day",length.out=n[j]/3)%>%rep(each=3))
  Data = rbind(Data,Data.aux)
  h = data.frame(Hora = rep(c("00:00","12:00","18:00"),n[j]/3))
  H =  rbind(H,h)
}
Esta = rep(Esta??es,n)
nrow(H)
H = H %>% as.matrix()%>% as.vector()
Grupos = (paste0("Group ",c(1,2,3,3,2,1,1,3,2,3,3,3,1,1)) %>% rep(n))
dados.brutos.2 = data.frame(Data,dados.brutos[,-1]) %>%
  mutate(UR = UR/100,
         Esta??es=Esta,
         Hora=H,
         M?s=format(Data,format="%m"),
         Ano=format(Data,format="%Y"),
         Dia=format(Data,format="%d")) %>%
  select(Esta??es,Data,Ano,M?s,Dia,Hora,everything())

head(dados.brutos.2)

dados.brutos.2 %>% group_by(M?s) %>% summarise(m=sum(PrecpTotal,na.rm = T))
dados.brutos.2 %>% select(Esta??es,Hora,M?s,UR,TempBulboSeco,TempBulboUmido) %>% na.omit()

#---- 3.1 Preenchendo os dados com datas e horas faltantes--------
Hora.datas.full = dados.brutos.2 %>%select(Esta??es,Data)%>% dplyr::group_by(Esta??es)%>%
  dplyr::summarise(na = seq.Date(as.Date("2000/01/01"),by = "day",length.out=max(n)/3)%>%rep(each=3) %in% Data,
                   Data = seq.Date(as.Date("2000/01/01"),by = "day",length.out=max(n)/3)%>%rep(each=3)) %>%
  mutate(Ano=format(Data,format="%Y"),
         M?s=format(Data,format="%m"),
         Dia=format(Data,format="%d")
         #H=rep(c("00:00","12:00","18:00"),(max(n)*L)/3)
  )
Hora.datas.full$Hora = rep(c("00:00","12:00","18:00"),(max(n)*L)/3)


#-------- 3.2 Identificando UR com na-------------
dados.bruto.3 = as.matrix(dados.brutos.2 %>% dplyr::select(UR,TempBulboSeco,TempBulboUmido,PrecpTotal))#vector(length = nrow(Hora.datas.full))
dados.bruto.4 = matrix(0,nrow(Hora.datas.full),ncol(dados.bruto.3))
aux.data = which(Hora.datas.full[,2]==T) # datas que j? est?o no banco de dados
dados.bruto.4[aux.data,]=dados.bruto.3
dados.bruto.4[-aux.data,]<-NA

dados.brutos.5 = data.frame(Hora.datas.full,UR = dados.bruto.4[,1],TBS=dados.bruto.4[,2],TBU=dados.bruto.4[,3],PrecpTotal=dados.bruto.4[,4])
#saveRDS(dados.brutos.5,"dados_brutos.RDS")
saveRDS(dados.brutos.5,"data_raw_1.RDS")
