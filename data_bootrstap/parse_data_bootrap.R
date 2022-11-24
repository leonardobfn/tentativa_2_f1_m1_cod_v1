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
for(i in 4:150){

path1 <- paste0("data_bootrstap/data_",i,"_boostrap.rds")
data_i <- readRDS(path1)
## Parse database------
month_names <- factor(month.abb, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
database <- data_i %>%
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
  u_lat_log <- dplyr::filter(data_i, City == u[j])[c("City", "Latitude", "Longitude")]
  lat_log <- unique(dados_aux[c("City", "Latitude", "Longitude")])
  aux <- rbind(lat_log, u_lat_log)
  w[j] <- weight(mun = aux$City, u_m = u[j], lat = as.numeric(aux$Latitude), long = as.numeric(aux$Longitude))
}
names(w) <- c(paste0("Group ", 1:length(u))) # u
w.data.frame <- tibble::enframe(w) %>%
  tidyr::unnest(cols = value) %>%
  select(value) %>%
  unlist() %>%
  rep(each = 252)
database <- database %>%
  arrange(Group) %>%
  mutate(weights = w.data.frame) %>%
  arrange(Group)

path <- paste0("data_bootrstap/","data_",i,"_boostrap.rds")
saveRDS(database,path)
}
#head(database)
#plot.ts(database$RH)
