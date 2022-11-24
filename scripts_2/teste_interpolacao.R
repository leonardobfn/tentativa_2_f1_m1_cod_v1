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
data.aux <- readRDS("scripts_2/1-estimations_Barcelos.rds")
city <- data.aux$data.complete %>% group_by(Group, City) %>% pull(City) %>% unique
results.full <- NULL
i = 8
path <- paste0("scripts_2/", i, "-estimations_", city[i], ".rds")
data.s0 <- readRDS(path)
s0 <- data.s0$s0
g <-data.s0$data.complete %>% filter(City == s0) %>% pull(Group) %>% unique()
y.s0 <- data.s0$data.complete %>% filter(City == s0)
y.sl.g <- data.s0$data.complete %>% filter(Group==g,City != s0)

delta <- data.s0$DADOS %>% data.frame() %>% slice(1:119)%>% pull(delta)
alfas <- data.s0$alpha$Estimate[as.numeric(str_sub(g,7))]

city.grupos <- unique(y.sl.g$City)
RH1 <- y.sl.g %>% filter(City==city.grupos[1]) %>% pull(RH)
RH2 <- y.sl.g %>% filter(City==city.grupos[2]) %>% pull(RH)

kt1 <- data.s0$DADOS %>% filter(idx==city.grupos[1]) %>% pull(kl_t)
kt2 <- data.s0$DADOS %>% filter(idx==city.grupos[2]) %>% pull(kl_t)

kt0 <- data.s0$link.kl$linkinv(model.matrix(Formula::Formula(data.s0$formula),y.s0,rhs=1)%*%
  data.s0$kl$Estimate)

phi1 <- log(kt1)/log(1-0.5^(delta))
phi2 <- log(kt2)/log(1-0.5^(delta))
phi0 <- log(kt0[-1])/log(1-0.5^(delta))

w1 <- y.sl.g %>% filter(City==city.grupos[1]) %>% pull(weights.w0) %>% unique()
w2 <- y.sl.g %>% filter(City==city.grupos[2]) %>% pull(weights.w0) %>% unique()
w0 <- y.s0  %>% pull(weights.w0) %>% unique()
ytil.1 <- -log(1-RH1[-1]^(1/phi1))
ytil.2 <- -log(1-RH2[-1]^(1/phi2))
b <- ytil.1*w1/delta
c <- ytil.2*w2/delta
n=20000
yt0.matrix <- matrix(0,119,n)
for(i in 1:n){
  u <- runif(1)
  ytil0 <- (((b+c)^alfas - log(1-u))^(1/alfas)-b-c)*delta/w0
  yt0 <- (1-exp(-ytil0))^phi0
  yt0.matrix[,i] <- yt0

}
est= apply(yt0.matrix, 1, median)
hist(est )
plot.ts(est,ylim=c(0.6,1),col="blue")
lines(y.s0$RH,col=1)

mean(abs(y.s0$RH[-1]-est)/y.s0$RH[-1])

