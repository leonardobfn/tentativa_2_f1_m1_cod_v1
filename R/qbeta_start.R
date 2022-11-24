qbeta_fit <- function(y, betas, quantil, link = link_kl, cov_kl = cov_kl) {
  betas <- matrix(betas)
  fx <- link$linkinv(cov_kl %*% betas)
  resid <- abs(y - fx)
  Q <- sum(quantil * resid[y >= fx]) + sum((1 - quantil) * resid[y < fx])
  return(sum(Q))
}
