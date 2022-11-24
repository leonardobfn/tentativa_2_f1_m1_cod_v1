GEV_kuma_fit <- function(teta,logrr=logr,vmm=vm,Espp=Esp,ncxx=nvc,ncvv=ncv,DADOS = DADOS,
                         Lmm = Lm,pp=p,qq=q,TTT=TT) {

  ar.ma <- teta[1:(pp + qq)]
  BETA <- teta[-c(1:(pp + qq))][1:ncxx]
  LAMBDA <- teta[-c(1:(pp + qq))][(ncxx + 1):(ncxx + ncvv)]
  alpha <- teta[-c(1:(pp + qq))][-c(1:(ncxx + ncvv))]

  wlog <- DADOS %>%
    select(grupos, idx, weights) %>%
    unique.data.frame() %>%
    group_by(grupos) %>%
    summarise(wlog = sum(log(weights))) %>%
    select(wlog) %>%
    unlist()

  Q1 <- sum(TTT * wlog - vmm * Espp[, 1] + logrr)
  Q2 <- sum(log(alpha / (1 - alpha)) - (1 / (1 - alpha)) * Espp[, 2] + Espp[, 3] - Espp[, 4] + TTT * Lmm * Espp[, 2])

  L <- Q1 + Q2

  return(-L)
}
