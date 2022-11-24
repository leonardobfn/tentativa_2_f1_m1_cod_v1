rmcem <- function(N, Lmm, vm, TT, alfastart) {
  # vm   <- ifelse(vm==0,.Machine$double.eps,vm)
  par_gama <- TT * Lmm - alfastart / (1 - alfastart) # parâmetro da distribuição gama
  f <- function(z) dgamma(z, par_gama, vm) # proposta
  z <- rgamma(N, par_gama, vm) # gerando da proposta
  B <- runif(N)
  phis <- purrr::map_dfr(z, phi, alfastart = alfastart, BB = B, TT = TT, Lmm = Lmm) # calculando os phis

  g1 <- phis[, 1] * z * dgamma(z, par_gama, vm) # g alvo
  num1 <- mean(g1 / f(z))
  g2 <- phis[, 1] * log(z) * dgamma(z, par_gama, vm) # g alvo
  num2 <- mean(g2 / f(z))
  g3 <- z^(-alfastart / (1 - alfastart)) * phis[, 3] * dgamma(z, par_gama, vm) # g alvo
  num3 <- mean(g3 / f(z))

  den <- mean(phis[, 1]) # phi0
  Em1 <- num1 / den
  Em2 <- num2 / den
  Em3 <- mean(phis[, 2]) / den
  Em4 <- num3 / den


  # den <- sum(phis[,1])#phi0
  # Em1 <- sum(z*phis[,1])/den
  # Em2 <- sum(log(z)*phis[,1])/den
  # Em3 <- sum(phis[,2])/den
  # Em4 <- sum(z^(-alfastart/(1-alfastart))*phis[,3])/den
  esp <- c(Em1, Em2, Em3, Em4)
  # esp[esp==Inf]<-.Machine$double.xmax#print(matrix(c(Em1,Em2,Em3,Em4),1,4))
  return(esp)
}
