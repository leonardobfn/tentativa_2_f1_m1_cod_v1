
rm(list=ls())
require(tidyverse)
path_data_raw_1 <- "data-raw/weather_stations/data_raw_1.RDS"
dados_brutos <- readRDS(path_data_raw_1)
head(dados_brutos)
dados_brutos$id = seq(1:nrow(dados_brutos))

na.ur = which(is.na(dados_brutos["UR"]==T))
# na.tbs = which(is.na(dados_brutos["TBS"]==T))
# na.tbu = which(is.na(dados_brutos["TBU"]==T))
# na.precp = which(is.na(dados_brutos["PrecpTotal"]==T))


# Estimação UR-------

beta_mom <- function(x) {

  m_x <- mean(x, na.rm = TRUE)
  s_x <- sd(x, na.rm = TRUE)

  alpha <- m_x*((m_x*(1 - m_x)/s_x^2) - 1)
  beta <- (1 - m_x)*((m_x*(1 - m_x)/s_x^2) - 1)

  return(list(estimate = c(alpha, beta)))

}

#estimacao <- dados_brutos %>%  slice(-c(na.ur)) %>% dplyr::group_by(Estações,Hora,Mês) %>% dplyr::summarise(m = median(sample(UR,length(UR),replace = T,prob = NULL)))

#estimacao <- dados_brutos %>%  slice(-c(na.ur)) %>% dplyr::group_by(Estações,Mês,Hora) %>% dplyr::summarise(m=MASS::fitdistr(UR,"gamma")$estimate%>%t())

estimacao <- dados_brutos %>%  slice(-c(na.ur)) %>% dplyr::group_by(Estações,Mês,Hora) %>% dplyr::summarise(m=beta_mom(UR)$estimate%>%t())

#dados_brutos[15744 ,]
#estimacao %>% dplyr::group_by(Estações,Hora,Mês) %>% summarise(m[,1]/m[,2])
path_ur_input <- "data-raw/weather_stations/ur_input.txt"
tictoc::tic()
for(i in 1:length(na.ur)){

  #estimacao <- dados_brutos %>%  slice(-c(na.tbu)) %>% dplyr::group_by(Estações,Hora,Mês) %>% dplyr::summarise(m = (sample(TBU,1,replace = T,prob = NULL)))
  #est <- estimacao %>% dplyr::filter(Estações==estimacao$Estações,Mês==estimacao$Mês,Hora==estimacao$Hora)  %>% mutate(UR_in=rgamma(100,m[,1],m[,2]) %>% median(trim=.10))
  dados_aux <- dados_brutos %>% slice(c(na.ur[i]))
  m <- estimacao %>% dplyr::filter(Estações==dados_aux$Estações,Mês==dados_aux$Mês,Hora==dados_aux$Hora)  %>% data.frame%>%select(m)
  UR_in <- rbeta(100,m$m[,1],m$m[,2]) %>% median()
  a <- data.frame(id=dados_aux$id,UR_in=UR_in)
  write.table(a,file = path_ur_input,append = T,row.names = F,col.names=F,sep=" ")
  #dados_aux <- dados_brutos %>% slice(c(na.ur)) %>% dplyr::filter(Estações==estimacao$Estações[i],Mês==estimacao$Mês[i],Hora==estimacao$Hora[i])  %>% mutate(UR_in=estimacao$m[i]) %>%
  #select(id,UR_in)
  # n_row <- nrow(dados_aux)
  # n_col <- ncol(dados_aux)
  #write.table(dados_aux,file = "ur.txt",append = T,row.names = F,col.names=F,sep=" ")
  cat(na.ur[i],i/length(na.ur)*100,"%","\r")
}
tictoc::toc()

# # Estimação TBU-------
#
# estimacao <- dados_brutos %>%  slice(-c(na.tbu)) %>% dplyr::group_by(Estações,Hora,Mês) %>% dplyr::summarise(m = median(sample(TBU,length(TBU),replace = T,prob = NULL)))
#
# tictoc::tic()
# for(i in 1:nrow(estimacao)){
#
#   #estimacao <- dados_brutos %>%  slice(-c(na.tbu)) %>% dplyr::group_by(Estações,Hora,Mês) %>% dplyr::summarise(m = (sample(TBU,1,replace = T,prob = NULL)))
#   dados_aux <- dados_brutos %>% slice(c(na.tbu)) %>% dplyr::filter(Estações==estimacao$Estações[i],Mês==estimacao$Mês[i],Hora==estimacao$Hora[i])  %>% mutate(tbu_in=estimacao$m[i]) %>%
#     select(id,tbu_in)
#   # n_row <- nrow(dados_aux)
#   # n_col <- ncol(dados_aux)
#   write.table(dados_aux,file = "tbu.txt",append = T,row.names = F,col.names=F,sep=" ")
#   cat(estimacao$Estações[i],i/nrow(estimacao)*100,"%","\r")
# }
# tictoc::toc()
#

# Estimação TBS-------

# estimacao <- dados_brutos %>%  slice(-c(na.tbs)) %>% dplyr::group_by(Estações,Hora,Mês) %>% dplyr::summarise(m = median(sample(TBS,length(TBS),replace = T,prob = NULL)))
#
# tictoc::tic()
# for(i in 1:nrow(estimacao)){
#
#   #estimacao <- dados_brutos %>%  slice(-c(na.tbu)) %>% dplyr::group_by(Estações,Hora,Mês) %>% dplyr::summarise(m = (sample(TBU,1,replace = T,prob = NULL)))
#   dados_aux <- dados_brutos %>% slice(c(na.tbs)) %>% dplyr::filter(Estações==estimacao$Estações[i],Mês==estimacao$Mês[i],Hora==estimacao$Hora[i])  %>% mutate(tbs_in=estimacao$m[i]) %>%
#     select(id,tbs_in)
#   # n_row <- nrow(dados_aux)
#   # n_col <- ncol(dados_aux)
#   write.table(dados_aux,file = "tbs.txt",append = T,row.names = F,col.names=F,sep=" ")
#   cat(estimacao$Estações[i],i/nrow(estimacao)*100,"%","\r")
# }
# tictoc::toc()
#
#
# # Estimação Precipitação-------
#
# estimacao <- dados_brutos %>%  slice(-c(na.precp)) %>% dplyr::group_by(Estações,Hora,Mês) %>% dplyr::summarise(m = max(sample(PrecpTotal,length(PrecpTotal),replace = T,prob = NULL)))
#
# tictoc::tic()
# for(i in 1:nrow(estimacao)){
#
#   #estimacao <- dados_brutos %>%  slice(-c(na.tbu)) %>% dplyr::group_by(Estações,Hora,Mês) %>% dplyr::summarise(m = (sample(TBU,1,replace = T,prob = NULL)))
#   dados_aux <- dados_brutos %>% slice(c(na.precp)) %>% dplyr::filter(Estações==estimacao$Estações[i],Mês==estimacao$Mês[i],Hora==estimacao$Hora[i])  %>% mutate(Precp_in=estimacao$m[i]) %>%
#     select(id,Precp_in)
#   # n_row <- nrow(dados_aux)
#   # n_col <- ncol(dados_aux)
#   write.table(dados_aux,file = "precep.txt",append = T,row.names = F,col.names=F,sep=" ")
#   cat(estimacao$Estações[i],i/nrow(estimacao)*100,"%","\r")
# }
# tictoc::toc()


