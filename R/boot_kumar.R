boot_kumar <- function(idx, n, B, mtrim) {
  # rm(list=ls())
  # require(extraDistr)
  # require(tidyverse)
  # n <- 1000
  # B <- 1
  # mtrim <- 0
  # devtools::load_all()
  path_estimation <- "estimations_predicts/estimations.rds"
  results <- readRDS(path_estimation)

  p <- results$p
  q <- results$q
  row.deleted <- (1:(max(q, p)))
  data <- results$data %>%
    arrange(Group) %>%
    group_by(City) # %>% slice(-(1:(max(q,p))))
  quantil <- results$quantil

  cov_kl <- results$cov_kl
  cov_delta <- results$cov_delta
  ncx <- ncol(cov_kl)
  ncv <- ncol(cov_delta)
  alfa.est <- results$alpha$Estimate

  est <- results$DADOS %>%
    data.frame() %>%
    mutate(
      bq_t = 1 / delta,
      phi_ts = log(kl_t) / log(1 - (1 - quantil)^(delta)),
      aq_t = 1 / phi_ts
    ) %>%
    arrange(grupos)

  yest.aux <- matrix(0, nrow = nrow(est), ncol = B)

  for (B in 1:B) {
    estt <- NULL
    for (j in 1:length(alfa.est)) {

      # qest=NULL
      G <- paste("Group", j)
      alfa.aux <- alfa.est[j]

      #set.seed(10)
      e <- est %>% dplyr::filter(grupos==G) %>% mutate(zm = rstable_pos(n,alfa.aux) %>% mean(trim=mtrim),
                                                       H=zm*weights,
                                                       Espy_z= bq_t*H*beta(1+1/aq_t,bq_t*H),
                                                       Vary_z= bq_t*H*beta(1+2/aq_t,bq_t*H)-(bq_t*H*beta(1+1/aq_t,bq_t*H))^2,
                                                       betaa=rbeta(1,H,1),
                                                       yest = (1-betaa^(delta))^(phi_ts),
                                                       ycond = (1-(1-quantil)^(1/(bq_t*H)))^(1/aq_t),
                                                       ymarg = (1-exp(-(delta/weights)*(-log(1-quantil))^(1/alfa.aux)))^phi_ts,
                                                       Fy_z = pkumar(.7,aq_t,bq_t*zm*weights,lower.tail = F),
                                                       r_qt = qnorm(pkumar(y,aq_t,bq_t*zm*weights,lower.tail = F)),
                                                       cofvar=sqrt(Vary_z)/Espy_z,
      )


      estt=rbind(estt,e)
    }
    yest.aux[, B] <- estt$yest
  }
  qest <- apply(yest.aux, 1, quantile, prob = c(0.5))
  data$id <- seq(1, nrow(data))
  row.deleted <- data %>%
    group_by(Group, City) %>%
    slice((1:(max(q, p)))) %>%
    data.frame() %>%
    select(id) %>%
    unlist()
  data[-row.deleted, "RH"] <- qest
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
  # z1 = results$data %>% filter(City=="Manaus") %>% select(RH)
  # z2 = data %>%filter(City=="Manaus") %>% select(RH)
  # plot(1:120,z1$RH[1:120],type="o")
  # lines(1:120,z2$RH[1:120],type="o",col=2)
  N <- results$N
  erro <- results$erro
  formula <- results$formula
  w <- data$weights
  link_kl <- results$link.kl
  link_delta <- results$link.delta
  # tetastart <- c(ar,betas,lambdas,alfas)

  mod <- try(gev_kuma(
    formula = formula, link.kl = link_kl$name, link.delta = link_delta$name, w = w, data = data, N = N,
    grupos = data$Group, quantil = quantil, erro = erro, idx = data$City, p = p, q = q
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
    paste0(colnames(cov_kl), "_kl"), paste0(colnames(cov_delta), "_delta"), paste0("alpha_", 1:length(alfa.est))
  )

  estimation <- data.frame(Estimation = unlist(c(ar_est, betas_est, lambdas_est, alfas_est)), Par = names)
  path_monte_carlo <- "estimations_predicts/estimation_mc.txt"
  write.table(estimation, file = path_monte_carlo, append = T, quote = T, col.names = F)
  # return(estimation)
}
