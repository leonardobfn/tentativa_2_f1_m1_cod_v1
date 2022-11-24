GEV_kuma_fit_lambda <- function(teta, alpha, BETA, ar.ma, cov_kl = cov_kl, cov_delta = cov_delta,
                                DADOS = DADOS, link_kl = link_kl, link_delta = link_delta,
                                Lm = Lm, quantil = quantil, Esp = Esp, p, q) {
  LAMBDA <- teta
  gr <- levels((DADOS$grupos))
  vm <- logr <- vector(l = length(Lm))

  for (j in 1:length(Lm)) {
    data_group <- DADOS %>% dplyr::filter(grupos == gr[j])
    TT <- (length(data_group$y) / Lm[j]) - max(p,q)

    cov_delta_grupo <- as.matrix((data_group[c(paste0(colnames(cov_delta), "_delta"))]))
    eta_delta <- cov_delta_grupo %*% LAMBDA
    data_group <- data_group %>% mutate(delta = link_delta$linkinv(eta_delta))

    cov_kl_grupo <- as.matrix((data_group[c(colnames(cov_kl))]))
    eta_1 <- cov_kl_grupo %*% BETA
    eta_2 <- link_kl$linkfun(data_group$y) - eta_1

    kl_t <- data_group %>%
      mutate(eta_1 = eta_1, eta_2 = eta_2) %>%
      group_by(idx) %>%
      summarise(kl_t = klt(eta_1, eta_2, BETA = BETA, ar.ma, q = q, p = p, link_kl = link_kl, y.d = data_group$y)) %>%
      data.frame() %>%
      select(kl_t)

    data_group <- data_group %>%
      group_by(idx) %>%
      slice(-(1:max(p, q)))

    aux <- logr_vmteta(quantil = quantil, omega = data_group$weights, dados = data_group$y, kl = kl_t, delta = data_group$delta)
    vm[j] <- aux[1]
    logr[j] <- aux[2]
  }
  wlog <- DADOS %>%
    select(grupos, idx, weights) %>%
    unique.data.frame() %>%
    group_by(grupos) %>%
    summarise(wlog = sum(log(weights))) %>%
    select(wlog) %>%
    unlist()
  # B=Esp[,3]
  # CB = (((sin(pi*alpha*B)/sin(pi*B))^(1/(1-alpha)))*((sin(pi*(1-alpha)*B))/sin(pi*alpha*B)))
  # alpha = 1/(1+Esp[,2])
  # print(CB)
  Q1 <- sum(TT * wlog - vm * Esp[, 1] + logr)
  # print(Q1)
  # Q2 = sum(log(alpha/(1-alpha))-(1/(1-alpha))*Esp[,2]+log(CB) - CB*(Esp[,1]^(-alpha/(1-alpha))))
  Q2 <- sum(log(alpha / (1 - alpha)) - (1 / (1 - alpha)) * Esp[, 2] + Esp[, 3] - Esp[, 4] + TT * Lm * Esp[, 2])


  # deltak = ifelse(it <= c*W,1,1/(it-c*W))
  # s = (Q1+Q2)
  # L = (Lant + deltak*(s - Lant))
  # print(c(s))
  L <- Q1 + Q2

  return(-L)
}
