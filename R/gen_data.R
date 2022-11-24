gen.data <- function(data.){
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
  dados <- data. %>%
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
  path_map_am <- "map_AM/13mu2500gc.shp"
  am<-readOGR(path_map_am)
  am@data[,2] = iconv(am@data[,2], "UTF-8","latin1")
  am@data[10,2] = "SGC"
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

  data_1 <- dados
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
  #head(database)

   #head(dados)
  #data_1 <- dados
  #path <- paste0("data_bootrstap/","data_",i,"_boostrap.rds")
  return(database)
  #saveRDS(dados,path)
  #usethis::use_data(data_1,overwrite = T)
}

