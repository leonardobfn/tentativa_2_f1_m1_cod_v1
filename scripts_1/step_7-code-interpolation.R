# install.packages('rgeos', type='source')
# install.packages('rgdal', type='source')
rm(list=ls())
require(rgeos)
require(sp)
require(maptools)
require(rgdal)
require(spatstat)
require(kriging)
require(tidyverse)
devtools::load_all()
#"UTF-8","latin1"

path_estimation <- "estimations_predicts/estimations.rds"
results <- readRDS(path_estimation)
#full_join(x = data.frame(a=c(1,2)),y=data.frame(a=c(3,4)))

#---- fitting the lm model to log(altitude)------

# data("data_1")
# dados <- data_1
p = results$p
q = results$q
dados <- results$data %>% filter(Station==1) %>% data.frame()%>%arrange(Group)%>%group_by(City) %>% slice(-(1:(max(q,p))))
nrow(dados)/14
alt_dados <- dados %>% select(City,Altitude,Latitude,Longitude ) %>% unique.data.frame()
model.alt <- lm(log(Altitude)~as.numeric(Longitude )+as.numeric(Latitude),data = alt_dados)

path.map <- "map_AM/13mu2500gc.dbf"
am <- readOGR(path.map,encoding ="UTF-8")
plot(am)
coordinates(am)
ID = cut(coordinates(am)[,1],quantile(coordinates(am)[,1]),include.lowest=T)
isTRUE(gpclibPermitStatus())
novo_mapa = unionSpatialPolygons(am,ID)
plot(novo_mapa)
mapa_dis = gUnaryUnion(novo_mapa)
Longitude  <- mapa_dis@polygons[[1]]@Polygons[[1]]@coords[,1]
Latitude <- mapa_dis@polygons[[1]]@Polygons[[1]]@coords[,2]
plot(Longitude,Latitude,pch="-")
pontos_interpolcao <- cbind(Longitude ,Latitude) %>% data.frame()
alt_est <- predict(model.alt,newdata=pontos_interpolcao) %>% data.frame() # predict of altitude
poly <- list(data.frame(pontos_interpolcao))
#x11()
plot(am)
points(pontos_interpolcao,cex=2,col=2)
text(pontos_interpolcao,cex=.9)

p = c(2210,2100,831:1292,675,1300:1700,2249:2304) %>% sort() # selecionando alguns pontos para inputar dados
grupo <- NULL
dist_min <-distt<- NULL

## Calculando as distâncias/omega dos pontos_interpolação e os ui's------
u1 = "Nova Olinda do Norte";u2="Maraã";u3="Itamarati"
u = c(u1,u2,u3)
long.lat.u <- am@data %>% filter(NOME %in%u) %>% mutate(Latitude=LATITUDE,Longitude=LONGITUDE)%>%select(NOME,Latitude ,Longitude )
pontos_interpolcao <- pontos_interpolcao[p,]
for(j in 1:nrow(pontos_interpolcao)){
  for(i in 1:nrow(long.lat.u)){

    dist_min[i] <- dist(rbind(long.lat.u[i,-1],pontos_interpolcao[j,]))
  }
  grupo[j]<- paste0("Group ",which.min(dist_min))
  aux <- data.frame(distt=dist_min[which.min(dist_min)]%>%data.frame(),
                    idx=j,Latitude=pontos_interpolcao[j,2],Longitude=pontos_interpolcao[j,1])
  distt <- rbind(distt,aux)

}
length(grupo)
nrow(distt)

omega <- function(x){
  tau_m = median(x)#.5#max(distt)
  C_n = exp((-0.5*(x)^2)/tau_m^2)
  omega = C_n/sum(C_n)
  return(omega)
}

TT=nrow(dados)/14
n <- length(Latitude[p])
om <-data.frame(grupo,distt) %>% rename(d=".") %>% arrange(grupo)
head(om)
nrow(om)
om1 <- om %>% group_by(grupo) %>% summarise(peso=omega(d))
len.gr <- om %>% group_by(grupo) %>% summarise(n=length(grupo))%>% pull(n)
om1 <- om1 %>% group_by(grupo) %>% summarise(peso=rep(peso,each=TT)) %>% data.frame()
nrow(om1)
#results <- readRDS("dados_resultados\\estimations.rds")
#database <- results$data
#y.min <- dados %>% group_by(Group,Year,month_names) %>% summarise(nn=length(RH),y.min=mean(sample(RH,nn-1,prob = NULL)))


res = NULL
for(j in 1:3){
  #j=2
  g<-paste0("Group ",j)
  aux <- dados %>%
    filter(Group==g) %>%
    group_by(Group,Year,month_names,Month) %>%
    summarise(nn=length(RH),
              yest=replicate(len.gr[j],mean(sample(RH,nn-1,prob = NULL)))
    ) %>%
    select(yest)


  idx=rep(seq(1,len.gr[j]),TT)
  aux <- aux %>% data.frame() %>% mutate(idx=idx)%>% arrange(idx)
  # aux1 <- y.min %>% filter(Group==g)
  # aux2 <- apply(aux1,2,rep,len.gr[j])
  res <- rbind(res,aux %>% data.frame())
}
om2 <- apply(om,2,rep,119) %>% data.frame() %>% arrange(grupo,idx) %>% mutate(yest=res$yest) %>%
  select(-d) %>%  rename(grupos=grupo) %>% mutate(Ano=res$Year,Mês=res$month_names,month=res$Month)%>% rename_with(str_to_lower)

head(om2)
value.predic <- readRDS("estimations_predicts/predicted_values.rds")
head(value.predic)
value.predic <- value.predic %>%
  mutate(Month=dados$Month)%>%
  select(grupos,idx,LONGITUDE,LATITUDE,yest,Ano,Mês,Month) %>%
  rename_with(str_to_lower)

full_join(value.predic,om2)

predict.value <-full_join(value.predic,om2)
colnames(predict.value)

ano <- unique(predict.value$ano)[-1][9]
mes <- month.abb[c(1)]
value <- NULL
for(j in ano){
  for(k in mes){
    predict.aux <- predict.value %>% filter(ano == j & mês == k)
    t <- unique(predict.aux$month) %>% as.numeric()

    kriged <- kriging(predict.aux$longitude %>% as.numeric() ,
                      predict.aux$latitude %>% as.numeric(),
                      predict.aux$yest,
                      polygons=poly, pixels=300,lags = 10)

    name.file <- paste0("estimations_predicts/predicted_values_maps_",t,"_",k,"_",j,".rds")
    valores <- kriged$map
    valores$Ano <- rep(j,nrow(valores))
    valores$Mês <- rep(k,nrow(valores))
    #value <- rbind(value,valores)

    saveRDS(valores,name.file)

  }
}


plot(city[,1],city[,2])
maps <- value %>% ggplot(aes(x=x,y=y,z=pred))+
  metR::geom_contour_fill()+
  #metR::geom_contour_fill(color="white")+
  #geom_contour(color="white")+

  scale_x_continuous(expression(Longitude ),expand = c(0,0))+
  scale_y_continuous(expression(Latitude),expand = c(0,0))+
  #geom_sf_label(aes(label=idx))+
  #geom_line(data = city %>% data.frame(),aes(x = X1,y=X2))+
  # scale_fill_gradient((as.expression(paste("l(a,b;y)")))
  #                     ,low="black", high="gray",oob=squish,na.value = "white")

  scale_fill_gradientn("RH",
                       colours =lm.palette(30) %>% sort(decreasing = T) ,na.value = "white")+
  facet_grid(Ano~Mês)+
  theme_bw()+
  theme(legend.key.height = unit(2,"cm"))+
  geom_point(data =data.frame(x1=-62.924,x2=-0.975,pred="NA"),aes(y=x1,x=x2),colour="black")
x11()
maps
g# Analysis of results

## Packages
require(ggmap)
require(scales)
require(metR)
library(gganimate)
require(transformr)

years <- c(2000:2009)[-1][9]
month <- factor(month.abb,levels=c(month.abb[1:12]))[1:12]

data.base.pred <- NULL
for(j in years){
  for(k in month){
    t <- switch(k,
           Jan = 1,
           Feb = 2,
           Mar = 3,
           Apr = 4,
           May = 5,
           Jun = 6,
           Jul = 7,
           Aug = 8,
           Sep = 9,
           Oct = 10,
           Nov = 11,
           Dec = 12
    )
    name.file <- paste0("estimations_predicts\\predicted_values_maps_",t,"_",k,"_",j,".rds")
    data.base.aux <- readRDS(name.file)
    data.base.pred <- rbind(data.base.pred,data.base.aux)
  }
}
# coordinates(data.base.pred)<- ~x+y
# gridded(data.base.pred) = TRUE
#teste <- cbind(data.base.pred$x,data.base.pred$y) %>% as.polygonal()

library(RColorBrewer) # mix das cores
lm.palette <- colorRampPalette(c("lightblue","blue", "darkblue"), space = "rgb")
lm.palette<-colorRampPalette(c("#000099", "#00FEFF", "#45FE4F",
                               "#FCFF00", "#FF9400", "#FF3100"))

#lm.palette <- colorRampPalette(c("blues"), space = "rgb")
config.figure <- theme(legend.key.height = unit(0.5,"cm"),
                       legend.key.width = unit(3,"cm"),
                       axis.text =element_text(size = 10),
                       legend.position = "bottom"
)


maps <- data.base.pred %>%mutate(Mês=factor(Mês,levels = c(month.abb)))  %>% ggplot(aes(x=x,y=y,z=pred))+
  metR::geom_contour_fill()+
  #geom_contour(color="white")+
  scale_x_continuous(expression(Longitude ),expand = c(0,0))+
  scale_y_continuous(expression(Latitude),expand = c(0,0))+
  # scale_fill_gradient((as.expression(paste("l(a,b;y)")))
  #                     ,low="black", high="gray",oob=squish,na.value = "white")

  scale_fill_gradientn("RH",
                       colours =lm.palette(30) %>% sort(decreasing = T) ,na.value = "white")+
  facet_wrap(.~Mês)+
  theme_bw()+ config.figure

x11()
cairo_ps("Figures/maps.eps")
maps
dev.off()
# Video

library(RColorBrewer) # mix das cores
lm.palette <- colorRampPalette(c("lightblue","blue", "darkblue"), space = "rgb")
maps <- data.base.pred %>%mutate(M?s=factor(M?s,levels = c(month.abb)),
                                 Ano=as.character(Ano)) %>% arrange(Ano)%>%
  ggplot(aes(x=x,y=y,z=pred))+
  metR::geom_contour_fill()+
  geom_contour(color="white")+
  scale_x_continuous(expression(Longitude ),expand = c(0,0))+
  scale_y_continuous(expression(Latitude),expand = c(0,0))+
  # scale_fill_gradient((as.expression(paste("l(a,b;y)")))
  #                     ,low="black", high="gray",oob=squish,na.value = "white")

  scale_fill_gradientn("UR",
                       colours =lm.palette(20) ,na.value = "white")+
  facet_wrap(.~M?s)+
  theme_bw()+
  theme(legend.key.height = unit(2,"cm"))+
  transition_manual(Ano)+
  labs(title = "Year =  {current_frame}")

animate(maps,duration = 63,width = 800, height = 600,renderer = av_renderer()) #renderer = gifski_renderer() gif
anim_save("maps.mp4")



# maps of groups ----------------------------------------------------------

rm(list=ls())
require(rgdal) # leitura do map
require(sp) # plot do mapa
require(ggplot2)
require(rgeos)
require(tidyverse)
devtools::load_all()
data("data_1")
month_names <- factor(month.abb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
database <- data_1 %>% filter(Station=="1") %>%
  mutate(month_names = rep(month_names,21) %>% rep(14),
         data=paste0(Year,"/",month_names),
         t =seq(1,252) %>% rep(14),
         Group = rep(paste0("Group ",c(1,2,3,3,2,1,1,3,2,3,3,3,1,1)),each=252),
         Latitude  = scale( Latitude %>%as.numeric()),
         Longitude  = scale(Longitude %>%as.numeric()),
         cost = cos(2*pi*as.numeric(Month)/12),
         sent=sin(2*pi*as.numeric(Month)/12))

head(database)

path.map <- "map_AM/13mu2500gc.dbf"
am <- rgdal::readOGR(path.map,encoding ="UTF-8")
plot(am)

idx <- database%>% group_by(Group) %>% filter(Station==1) %>% pull(City) %>% unique()
idx[which(idx=="SGC")]<-c("São Gabriel da Cachoeira")

idx.1 <- database%>% filter(Station==1 & Group=="Group 1") %>% pull(City) %>% unique()
idx.1[which(idx.1=="SGC")]<-c("São Gabriel da Cachoeira")

idx.2 <- database%>% filter(Station==1 & Group=="Group 2") %>% pull(City) %>% unique()

idx.3 <- database%>% filter(Station==1 & Group=="Group 3") %>% pull(City) %>% unique()

names.city <- am[[2]]
stations.aux.1 <- am[(names.city%in%idx.1),]
stations.aux.2 <- am[(names.city%in%idx.2),]
stations.aux.3 <- am[(names.city%in%idx.3),]
no.stations.aux <- am[!(names.city%in%idx),]
col = gray.colors(3, start = 0.3, end = 0.9, rev = FALSE)


x11()
#cairo_ps("Figures/maps_groups.eps")
plot(no.stations.aux)
plot(stations.aux.1,add=T,col=col[1])
plot(stations.aux.2,add=T,col=col[2])
plot(stations.aux.3,add=T,col=col[3])
lat <- c(stations.aux.1@data$LATITUDE,stations.aux.2@data$LATITUDE,stations.aux.3@data$LATITUDE)
long<- c(stations.aux.1@data$LONGITUDE,stations.aux.2@data$LONGITUDE,stations.aux.3@data$LONGITUDE)
lat_u <-  c(am@data[str_detect(names.city,"Maraã"),]$LATITUDE,
            am@data[str_detect(names.city,"Nova Olinda do Norte"),]$LATITUDE,
            am@data[str_detect(names.city,"Itamarati"),]$LATITUDE)

long_u <-  c(am@data[str_detect(names.city,"Maraã"),]$LONGITUDE,
             am@data[str_detect(names.city,"Nova Olinda do Norte"),]$LONGITUDE,
             am@data[str_detect(names.city,"Itamarati"),]$LONGITUDE)

u1 <- expression(u[1])
u2 <- expression(u[2])
u3 <- expression(u[3])
text(x = c(long,long_u),
     y=c(lat,lat_u),
     labels=c(stations.aux.1@data$NOME,
              stations.aux.2@data$NOME,
              stations.aux.3@data$NOME,u1,u3,u2),cex=rep(c(2,2.5),c(13,3)),
     pos = rep(c(2,1),c(11,5)),
     col=rep(c(1,"red"),c(13,3)))
dev.off()




