boot_kumar_no_par <- function(idx, n, B, mtrim) {
  # rm(list=ls())
  # require(extraDistr)
  # require(tidyverse)
  # require(rgdal)
  # n <- 1000
  # B <- 1
  # mtrim <- 0.20
  # devtools::load_all()
  path_estimation <- "estimations_predicts/estimations.rds"
  results <- readRDS(path_estimation)
  data_full <- readRDS("data-raw/data_full.rds")
  database <- gen.data(data_full)
  p <- results$p
  q <- results$q
  quantil <- results$quantil
  TT1 <- results$TT + max(p,q) #
  data <- database %>%
    group_by(Group,City) %>%
    slice(1:TT1)
  N <- results$N
  erro <- results$erro
  formula <- results$formula
  w <- data$weights
  link_kl <- results$link.kl
  link_delta <- results$link.delta
  # rht <- results$data%>%group_by(Group,City) %>% summarise(rht=RH) %>%pull(rht)
  # dates <- seq.Date(as.Date("2000/01/01"),as.Date("2008/12/31"),by = "month",sep="/") %>% rep(14)
  # fig1 = data %>% data.frame()%>%mutate(dates=dates,rht = rht) %>%
  #   select(dates,RH,Group,City,rht)%>%pivot_longer(cols = c(rht,RH)) %>%dplyr::group_split(Group) %>%
  #   purrr::map(
  #     ~ggplot(.,aes(colour=name)) + geom_line(aes(dates,value),size=.8)+
  #       #geom_point(aes(t,value))+
  #       ylab("Relative humidity")+ #scale_y_continuous(labels=scales::percent)+
  #       scale_x_date(date_labels = "%b-%y",breaks = c(seq(as.Date("2000/1/1"), as.Date("2008/12/31"), by = "years")),limits = c(as.Date("2000/01/01"),as.Date("2008/12/31")))+
  #       #scale_x_discrete(labels = c(month.abb))+
  #       facet_grid(~City,scale="free")+
  #       theme_bw()+#geom_hline(data = d,aes(yintercept = kp))+
  #       #geom_hline(yintercept = q)+
  #       theme(legend.position = "none",axis.text.x=element_text(angle=60,size=8,hjust = 1),
  #             axis.title.y = element_text(size=8)))%>%
  #   cowplot::plot_grid(plotlist = .,nrow=3)
  # #x11()
  # fig1
  # tetastart <- c(ar,betas,lambdas,alfas)
  # formula = formula; link.kl = link_kl$name; link.delta = link_delta$name; w = w; data = data; N = N;
  # grupos = data$Group; quantil = quantil; erro = erro; idx = data$City; p = p; q = q
  #TT = nrow(data)/14
  mod <- try(gev_kuma(
    formula = formula, link.kl = link_kl$name, link.delta = link_delta$name, w = w, data = data, N = N,
    grupos = data$Group, quantil = quantil, erro = 50, idx = data$City, p = p, q = q
  ), TRUE)

  if (class(mod) == "try-error") {
    ar_est <- rep(NA, nrow(results$ar))
    betas_est <- rep(NA, nrow(results$kl))
    lambdas_est <- rep(NA, nrow(results$delta))
    alfas_est <- rep(NA, nrow(results$alpha))
  }
  if (class(mod) != "try-error") {
    ar_est <- mod$ar
    betas_est <- mod$kl
    lambdas_est <- mod$delta
    alfas_est <- mod$alpha
  }

  names <- c(
    results$ar %>% rownames(),
    paste0(rownames(mod$kl), "_kl"), paste0(rownames(mod$delta), "_delta"), paste0("alpha_", 1:nrow(alfas_est))
  )
browser()
  estimation <- data.frame(Estimation = unlist(c(ar_est, betas_est, lambdas_est, alfas_est)), Par = names)
  path_monte_carlo <- "estimations_predicts/estimation_mc.txt"
  write.table(estimation, file = path_monte_carlo, append = T, quote = T, col.names = F)

  log.like <- data.frame(log_like=mod$objective)
  path_log.like <- "estimations_predicts/log_like.txt"
  write.table(log.like, file = path_log.like, append = T, quote = T, col.names = F)
  # return(estimation)
}
