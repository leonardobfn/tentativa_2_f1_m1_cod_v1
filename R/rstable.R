
rstable_pos <- function(n, alfa) {
  U <- runif(n, 0, pi)
  v <- rexp(n, 1)
  a_U <- ((sin(alfa * U) / sin(U))^(1 / (1 - alfa))) * ((sin((1 - alfa) * U)) / sin(alfa * U))
  E <- (a_U / v)^((1 - alfa) / alfa)
  return(E)
}
