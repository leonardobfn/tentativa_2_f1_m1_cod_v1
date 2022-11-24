logr_vmteta <- function(quantil, omega, dados_grupo, kl, delta) {
  # omega = omega.aux[-c(id_eta_first)];kl=kl;delta=delta[-c(id_eta_first)];dados_grupo<-y_grupo[-c(id_eta_last)]
  dados_g <- data.frame(dados_grupo, kl, delta, omega)

  bq_t <- 1 / dados_g$delta # bq_t[bq_t==0] <- .Machine$double.eps

  phi_ts <- log(dados_g$kl) / log(1 - (1 - quantil)^dados_g$delta)

  # phi_ts[phi_ts<=0]=.Machine$double.eps

  aq_t <- 1 / phi_ts
  aq_t[aq_t <= 0] <- .Machine$double.eps

  p_kumar <- extraDistr::pkumar(dados_g$dados_grupo, a = aq_t, b = bq_t, lower.tail = F)
  p_kumar[p_kumar == 0] <- 1
  d_kumar <- extraDistr::dkumar(dados_g$dados_grupo, a = aq_t, b = bq_t)


  r <- d_kumar / p_kumar
  r[r == Inf | r == "NaN" | r == 0] <- 1
  vmteta <- -dados_g$omega * log(p_kumar)
  # print(c(sum(vmteta),sum(log(r))))
  return(c(sum(vmteta), sum(log(r))))
}
