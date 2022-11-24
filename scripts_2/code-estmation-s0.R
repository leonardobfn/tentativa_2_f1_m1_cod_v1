rm(list = ls())

# Packages -------
pack <- c("tidyverse", "extraDistr", "devtools", "Formula", "tictoc", "betareg", "cowplot")

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
month_names <- factor(month.abb, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
database <- data_1 %>%
  filter(Station == "1") %>%
  mutate(
    month_names = rep(month_names, 21) %>% rep(14),
    date = paste0(Year, "/", month_names),
    t = seq(1, 252) %>% rep(14),
    Group = rep(paste0("Group ", c(1, 2, 3, 3, 2, 1, 1, 3, 2, 3, 3, 3, 1, 1)), each = 252),
    cost1 = cos(2 * pi * as.numeric(Month) / 12),
    sent1 = sin(2 * pi * as.numeric(Month) / 12)
  )

head(database, 13)


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
  mutate(weights.w0 = w.data.frame) %>%
  arrange(Group)
head(database)
# Estimation ---------
TT <- 108 #
formula <- RH~log(Altitude) + sent1 + cost1 | sent1 + cost1
data <- database %>%
  group_by(Group,City) %>%
  slice(1:TT)
# data <- estimations$data

city <- data %>% pull(City)%>%unique()
tic <- tictoc::tic()
for(i in 1:length(city)){
  s0 <- city[i];id.s0 <- which(data$City==s0)
  data.set <- data %>% data.frame() %>% slice(-id.s0)
  w <- list()
  for (j in 1:3) {
    g <- paste0("Group ", j)
    dados_aux <- data.set %>% dplyr::filter(Group == g)
    u_lat_log <- dplyr::filter(data_1, City == u[j])[c("City", "Latitude", "Longitude")]
    lat_log <- unique(dados_aux[c("City", "Latitude", "Longitude")])
    aux <- rbind(lat_log, u_lat_log)
    w[j] <- weight(mun = aux$City, u_m = u[j], lat = as.numeric(aux$Latitude), long = as.numeric(aux$Longitude))
  }
  names(w) <- c(paste0("Group ", 1:length(u)))

  w.data.frame <- tibble::enframe(w) %>%
    tidyr::unnest(cols = value) %>%
    select(value) %>%
    unlist() %>%
    rep(each = TT)
  data.set <- data.set %>%
    arrange(Group) %>%
    mutate(weights = w.data.frame) %>%
    arrange(Group)


  quantil <- .5
  N <- 500
  erro <- 10^(-4)
  link.kl <- "loglog"
  link.delta <- "log"
  p <- 1 # AR(p)
  q <- 1 # MA(q)
  idx <- data.set$City
  x <- gev_kuma(
    formula = formula,
    p = p,
    q = q,
    grupos = data.set$Group,
    w = data.set$weights,
    N = N,
    quantil = .5,
    data = data.set,
    link.kl = "loglog", link.delta = "log", erro = erro,
    id = data.set$City
  )
  x$u <- u
  x$s0 <- s0
  x$data.complete <- data
  path_estimation <- paste0("scripts_2/",i,"-estimations_",s0,".rds")
  saveRDS(x, path_estimation) # saving the results
}
toc <- tictoc::toc()
beepr::beep(sound = 11, expr = NULL)
#7031.72 sec elapsed

