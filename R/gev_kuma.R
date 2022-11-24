

gev_kuma <- function(formula, p, q, link.kl = "loglog", link.delta = "log",
                     grupos, w, data, N, quantil, erro, idx) {
  mf <- model.frame(Formula::Formula(formula), data = data)
  y <- model.response(mf)
  cov_kl <- model.matrix(Formula::Formula(formula), data = data, rhs = 1)
  cov_delta <- model.matrix(Formula::Formula(formula), data = data, rhs = 2)

  ncx <- ncol(cov_kl)
  ncv <- ncol(cov_delta)
  grupos <- as.factor(grupos)
  gr <- levels(grupos)
  DADOS <- data.frame(idx, y, cov_kl, cov_delta, grupos, w) %>% arrange(grupos)
  colnames(DADOS) <- c(
    "idx", "y",
    colnames(cov_kl),
    paste0(colnames(cov_delta), "_delta"),
    "grupos", "weights"
  )

  Lm <- DADOS %>%
    group_by(grupos) %>%
    summarise(Lm = length(unique(idx))) %>%
    select(Lm) %>%
    unlist() # number of station per group
  vm <- logr <- vector(l = length(gr))
  Esp <- matrix(0, length(gr), 4)


  if (link.kl != "loglog") {
    link_kl <- make.link(link.kl)
  }
  if (link.kl == "loglog") {
    link_kl <- structure(list(
      linkfun = function(mu) -log(-log(mu)),
      linkinv = function(eta) {
        pmax(pmin(
          exp(-exp(-eta)),
          1 - .Machine$double.eps
        ), .Machine$double.eps)
      },
      mu.eta = function(eta) {
        eta <- pmin(eta, 700)
        pmax(exp(-eta - exp(-eta)), .Machine$double.eps)
      }, dmu.deta = function(eta) {
        pmax(exp(-exp(-eta) -
                   eta) * expm1(-eta), .Machine$double.eps)
      }, valideta = function(eta) TRUE,
      name = "loglog"
    ), class = "link-glm")
  }

  # starts param ----------

  if (link.delta == "identity" || link.delta == "log" || link.delta == "sqrt") {
    link_delta <- make.link(link.delta)
  }
  # else{stop("arg should be one of identity, log, sqrt ")}
  if (link.delta == "identity" || link.delta == "log" || link.delta == "sqrt") {
    link_delta <- make.link(link.delta)
  }
  # else{stop("arg should be one of identity, log, sqrt ")}

  if (p != 0 & q != 0) {
    ar.ma.start <- coef(arima(y, c(p, 0, q)), include.mean = T)[1:(p + q)]
    # ar_start <- ar.ma.start[1:p]
    # ma_start <- ar.ma.start[-(1:p)]
  }

  if (p != 0 & q == 0) {
    ar.ma.start <- coef(arima(y, c(p, 0, q)), include.mean = T)[1:(p)]
    # ar_start <- ar.ma.start[1:p]
  }

  if (p == 0 & q != 0) {
    ar.ma.start <- coef(arima(y, c(p, 0, q)), include.mean = T)[1:(q)]
    # ar_start <- ar.ma.start[1:p]
  }

  beta_start <- optim(c(rep(0, ncx)), qbeta_fit, quantil = quantil, control = list(maxit = 600), link = link_kl, cov_kl = cov_kl, y = y)$par
  delta_start <- -betareg::betareg(
    formula = formula, data = data,
    link.phi = "log", link = "loglog"
  )$coefficients$precision



  # <<<<<<< Updated upstream
  # alpha_start <- c(.9,.9,.9)
  # tetastart <- c(ar.ma.start,beta_start,delta_start,alpha_start)

  alpha_start <- c(.9, .9, .9)
  tetastart <- c(ar.ma.start, beta_start, delta_start, alpha_start)
  # tetastart<-c(2.758857892, 2.459888282,  2.368689001,  2.335887240,  2.222884843,  -2.370466897,  -2.339194846 , -0.289725650 , -0.323093489 ,  1.792263566,  -0.003936816  ,
  #      0.490299716,  -0.287023293,
  #      -1.303329283,   0.794150240,   0.035349107,   0.127991855,   0.136733141,   0.118221108)
  # >>>>>>> Stashed changes
  tetastart_p <- tetastart
  EMVteta <- EMVteta.aux <- list()
  cont = 0
  # crit_value <- c(1000)
  # EMV----------------
  repeat{
    ## Step E-----------------
    ar.ma <- tetastart[1:(p + q)]
    BETA <- tetastart[-c(1:(p + q))][1:ncx]
    LAMBDA <- tetastart[-c(1:(p + q))][(ncx + 1):(ncx + ncv)]
    alpha <- tetastart[-c(1:(p + q))][-c(1:(ncx + ncv))]

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
      Esp[j, ] <- rmcem(N, Lmm = Lm[j], vm = vm[j], TT = TT, alfastart = alpha[j])
    }

    ## Step M------

    #EMVteta_alpha <- (1 / (1 + Esp[, 2]))
    #EMVteta_alpha <- (link_kl$linkfun(1-quantil)) / (link_kl$linkfun(1-quantil) + Esp[, 2])
    #quantil <- 1-link_kl$linkfun((Esp[, 2]*EMVteta_alpha)/(EMVteta_alpha-1))

    EMVteta_alpha <- try(optim(
      par = alpha,BETA=BETA, LAMBDA = LAMBDA, ar.ma = ar.ma,
      fn = GEV_kuma_fit_alpha, method = "L-BFGS-B", control = list(maxit = 700),
      quantil = quantil, cov_kl = cov_kl, cov_delta = cov_delta, DADOS = DADOS, link_kl = link_kl, link_delta = link_delta, Lm = Lm,
      Esp = Esp, p = p, q = q,lower=c(rep(0.01,length(alpha))),upper = c(rep(0.98,length(alpha)))
    ), TRUE)


    aux.alpha <- which(EMVteta_alpha$par>=1)
    if(length(aux.alpha>=1)){
      EMVteta$par <- c(ar.ma, BETA,LAMBDA,alpha)
      names(EMVteta$par) <- c(
        names(ar.ma.start),
        colnames(cov_kl),
        colnames(cov_delta),
        paste0("alpha_", 1:length(alpha_start))
      )

      cat("Estimates:", sep = "\n")
      print(EMVteta$par)
      cat("Crit:", sep = "\n")
      print("AA")
      warning("Next Em2 < 0")


      break


    }

    # for (j in 1:length(Lm)) {
    #   data_group <- DADOS %>% dplyr::filter(grupos == gr[j])
    #   TT <- (length(data_group$y) / Lm[j]) - max(p,q)
    #
    #   cov_delta_grupo <- as.matrix((data_group[c(paste0(colnames(cov_delta), "_delta"))]))
    #   eta_delta <- cov_delta_grupo %*% LAMBDA
    #   data_group <- data_group %>% mutate(delta = link_delta$linkinv(eta_delta))
    #
    #   cov_kl_grupo <- as.matrix((data_group[c(colnames(cov_kl))]))
    #   eta_1 <- cov_kl_grupo %*% BETA
    #   eta_2 <- link_kl$linkfun(data_group$y) - eta_1
    #
    #   kl_t <- data_group %>%
    #     mutate(eta_1 = eta_1, eta_2 = eta_2) %>%
    #     group_by(idx) %>%
    #     summarise(kl_t = klt(eta_1, eta_2, BETA = BETA, ar.ma, q = q, p = p, link_kl = link_kl, y.d = data_group$y)) %>%
    #     data.frame() %>%
    #     select(kl_t)
    #
    #   data_group <- data_group %>%
    #     group_by(idx) %>%
    #     slice(-(1:max(p, q)))
    #
    #   aux <- logr_vmteta(quantil = quantil, omega = data_group$weights, dados = data_group$y, kl = kl_t, delta = data_group$delta)
    #   vm[j] <- aux[1]
    #   logr[j] <- aux[2]
    #   Esp[j, ] <- rmcem(N, Lmm = Lm[j], vm = vm[j], TT = TT, alfastart = EMVteta_alpha$par[j])
    # }


    try_error <- 0
    EMVteta_beta <- try(optim(
      par = BETA, LAMBDA = LAMBDA, alpha = EMVteta_alpha$par, ar.ma = ar.ma,
      fn = GEV_kuma_fit_beta, method = "L-BFGS-B", control = list(maxit = 700),
      quantil = quantil, cov_kl = cov_kl, cov_delta = cov_delta, DADOS = DADOS, link_kl = link_kl, link_delta = link_delta, Lm = Lm,
      Esp = Esp, p = p, q = q
    ), TRUE)

    if (class(EMVteta_beta) == "try-error") {
      EMVteta_beta <- list(par = beta_start)
      try_error <- 1
    }

    # for (j in 1:length(Lm)) {
    #   data_group <- DADOS %>% dplyr::filter(grupos == gr[j])
    #   TT <- (length(data_group$y) / Lm[j]) - max(p,q)
    #
    #   cov_delta_grupo <- as.matrix((data_group[c(paste0(colnames(cov_delta), "_delta"))]))
    #   eta_delta <- cov_delta_grupo %*% LAMBDA
    #   data_group <- data_group %>% mutate(delta = link_delta$linkinv(eta_delta))
    #
    #   cov_kl_grupo <- as.matrix((data_group[c(colnames(cov_kl))]))
    #   eta_1 <- cov_kl_grupo %*% EMVteta_beta$par
    #   eta_2 <- link_kl$linkfun(data_group$y) - eta_1
    #
    #   kl_t <- data_group %>%
    #     mutate(eta_1 = eta_1, eta_2 = eta_2) %>%
    #     group_by(idx) %>%
    #     summarise(kl_t = klt(eta_1, eta_2, BETA = EMVteta_beta$par, ar.ma, q = q, p = p, link_kl = link_kl, y.d = data_group$y)) %>%
    #     data.frame() %>%
    #     select(kl_t)
    #
    #   data_group <- data_group %>%
    #     group_by(idx) %>%
    #     slice(-(1:max(p, q)))
    #
    #   aux <- logr_vmteta(quantil = quantil, omega = data_group$weights, dados = data_group$y, kl = kl_t, delta = data_group$delta)
    #   vm[j] <- aux[1]
    #   logr[j] <- aux[2]
    #   Esp[j, ] <- rmcem(N, Lmm = Lm[j], vm = vm[j], TT = TT, alfastart = EMVteta_alpha$par[j])
    # }


    EMVteta_ar <- try(optim(
      par = ar.ma, LAMBDA = LAMBDA, alpha = EMVteta_alpha$par, BETA = EMVteta_beta$par,
      quantil = quantil, fn = GEV_kuma_fit_ar, lower = rep(-.99, p + q), upper = rep(.99, p + q), method = "L-BFGS-B", control = list(maxit = 700),
      cov_kl = cov_kl, cov_delta = cov_delta, DADOS = DADOS, link_kl = link_kl, link_delta = link_delta, Lm = Lm,
      Esp = Esp, p = p, q = q
    ), TRUE)


    if (class(EMVteta_ar) == "try-error") {
      EMVteta_ar <- list(par = ar.ma.start)
      try_error <- 1
    }

    # for (j in 1:length(Lm)) {
    #   data_group <- DADOS %>% dplyr::filter(grupos == gr[j])
    #   TT <- (length(data_group$y) / Lm[j]) - max(p,q)
    #
    #   cov_delta_grupo <- as.matrix((data_group[c(paste0(colnames(cov_delta), "_delta"))]))
    #   eta_delta <- cov_delta_grupo %*% LAMBDA
    #   data_group <- data_group %>% mutate(delta = link_delta$linkinv(eta_delta))
    #
    #   cov_kl_grupo <- as.matrix((data_group[c(colnames(cov_kl))]))
    #   eta_1 <- cov_kl_grupo %*% EMVteta_beta$par
    #   eta_2 <- link_kl$linkfun(data_group$y) - eta_1
    #
    #   kl_t <- data_group %>%
    #     mutate(eta_1 = eta_1, eta_2 = eta_2) %>%
    #     group_by(idx) %>%
    #     summarise(kl_t = klt(eta_1, eta_2, BETA = EMVteta_beta$par, EMVteta_ar$par, q = q, p = p, link_kl = link_kl, y.d = data_group$y)) %>%
    #     data.frame() %>%
    #     select(kl_t)
    #
    #   data_group <- data_group %>%
    #     group_by(idx) %>%
    #     slice(-(1:max(p, q)))
    #
    #   aux <- logr_vmteta(quantil = quantil, omega = data_group$weights, dados = data_group$y, kl = kl_t, delta = data_group$delta)
    #   vm[j] <- aux[1]
    #   logr[j] <- aux[2]
    #   Esp[j, ] <- rmcem(N, Lmm = Lm[j], vm = vm[j], TT = TT, alfastart = EMVteta_alpha$par[j])
    # }
    EMVteta_lambda <- try(optim(
      par = LAMBDA, BETA = EMVteta_beta$par, ar.ma = EMVteta_ar$par, alpha = EMVteta_alpha$par,
      fn = GEV_kuma_fit_lambda, method = "L-BFGS-B", control = list(maxit = 700),
      quantil = quantil, cov_kl = cov_kl, cov_delta = cov_delta, DADOS = DADOS, link_kl = link_kl, link_delta = link_delta, Lm = Lm,
      Esp = Esp, p = p, q = q
    ), TRUE)

    if (class(EMVteta_lambda) == "try-error") {
      EMVteta_lambda <- list(par = delta_start)
      try_error <- 1
    }


    EMVteta$par <- c(EMVteta_ar$par, EMVteta_beta$par, EMVteta_lambda$par, EMVteta_alpha$par)
    names(EMVteta$par) <- c(
      names(ar.ma.start),
      colnames(cov_kl),
      colnames(cov_delta),
      paste0("alpha_", 1:length(alpha_start))
    )
    # colocar aqui L(EMVteta$par)


    crit <- sum(((EMVteta$par - tetastart) / tetastart)^2)
    if(crit<erro){
    cont = cont + 1
    EMVteta.aux <- rbind(EMVteta.aux,EMVteta$par)
    }

    cat("Estimates:", sep = "\n")
    print(EMVteta$par)
    cat("Crit:", sep = "\n")
    print(crit)
    if (cont == 3) {
      break
    } else {
      tetastart <- EMVteta$par
    }

  }

  EMVteta$par <- apply(EMVteta.aux,2,as.numeric) %>% colMeans()
  ar.ma <- EMVteta$par[1:(p + q)]
  BETA <- EMVteta$par[-c(1:(p + q))][1:ncx]
  LAMBDA <- EMVteta$par[-c(1:(p + q))][(ncx + 1):(ncx + ncv)]
  alpha <- EMVteta$par[-c(1:(p + q))][-c(1:(ncx + ncv))]


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
    Esp[j, ] <- rmcem(N, Lmm = Lm[j], vm = vm[j], TT = TT, alfastart = alpha[j])
  }

  EMVteta$value <- GEV_kuma_fit(EMVteta$par,TTT=TT,ncxx=ncx,ncvv=ncv,logrr=logr,vmm=vm,Espp=Esp,DADOS=DADOS,Lmm=Lm,pp=p,qq=q)

  nalfa <- length(Lm)


  ar.ma_par <- data.frame(EMVteta$par[c(1:(p + q))])
  rownames(ar.ma_par) <- names(ar.ma.start)
  colnames(ar.ma_par) <- "Estimate"
  kl_par <- data.frame(EMVteta$par[-c(1:(p + q))][1:ncx])
  rownames(kl_par) <- colnames(cov_kl)
  colnames(kl_par) <- "Estimate"
  delta_par <- data.frame(EMVteta$par[-c(1:(p + q))][(ncx + 1):(ncx + ncv)])
  rownames(delta_par) <- colnames(cov_delta)
  colnames(delta_par) <- "Estimate"
  alpha_par <- data.frame(EMVteta$par[-c(1:(p + q))][-c(1:(ncx + ncv))])
  rownames(alpha_par) <- paste0("alpha_", 1:nalfa)
  colnames(alpha_par) <- "Estimate"

  eta_1 <- as.matrix(DADOS[colnames(cov_kl)])%*% (kl_par[, 1])
  eta_2 <- link_kl$linkfun(DADOS$y) - eta_1

  kl_t <- DADOS %>%
    mutate(eta_1 = eta_1, eta_2 = eta_2) %>%
    group_by(grupos, idx) %>%
    summarise(klt = klt(eta_1, eta_2, BETA = as.matrix(kl_par)[1, 1], ar.ma = as.matrix(ar.ma_par), q = q, p = p, link_kl = link_kl, y.d = DADOS$y)) %>%
    data.frame() %>%
    select(klt) %>%
    unlist()


  DADOS <- DADOS %>%
    mutate(delta = link_delta$linkinv(as.matrix(DADOS[c(paste0(colnames(cov_delta), "_delta"))]) %*% delta_par[,1])) %>%
    group_by(grupos, idx) %>%
    slice(-(1:(max(p, q))))
  DADOS$kl_t <- kl_t

  res <- list(
    ar = ar.ma_par,
    kl = kl_par,
    delta = delta_par,
    alpha = alpha_par,
    quantil = quantil,
    data = data,
    cov_delta = cov_delta,
    cov_kl = cov_kl,
    link.kl = link_kl,
    link.delta = link_delta,
    convergence = list(
      EMVteta_alpha_convergence = EMVteta_alpha$convergence,
      EMVteta_ar_convergence = EMVteta_ar$convergence,
      EMVteta_beta_convergence = EMVteta_beta$convergence,
      EMVteta_lambda_convergence = EMVteta_lambda$convergence
    ),
    objective=EMVteta$value,
    formula = formula,
    Call = match.call(),
    w = w,
    N = N,
    erro = erro,
    TT =  (nrow(cov_kl)/sum(Lm))-max(p,q),
    tetastart_p = tetastart_p,
    p = p,
    q = q,
    DADOS = DADOS
  )
  class(res) <- "gev"
  return(res)
}
