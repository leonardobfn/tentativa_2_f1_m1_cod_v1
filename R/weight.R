
weight <- function(mun, u_m, lat, long) {
  u <- which(u_m == mun)
  pos <- cbind(lat, long)
  distt <- vector(length = nrow(pos))
  for (i in 1:nrow(pos))
  {
    distt[i] <- dist(rbind(pos[u, ], pos[i, ]))
  }
  distt <- distt[-u]
  tau_m <- median(distt) # .5#max(distt)
  #sig <- 1
  C_n <- exp((-0.5 * (distt)^2) / tau_m^2)
  #C_n <- exp(-(tau_m * (distt)^2))
  #C_n <- 1-sig*(1-(distt^2/(tau_m+distt^2)))
  #omega  <- 1-C_n
  #C_n <- 1/distt^(2)
  omega <- C_n/sum(C_n)
  names(omega) <- c(mun[-u])
  return(list(om = omega))
}

# tau_m=20
# tt = seq(0,100,by=.5)
# f <-   C_n <- exp((-0.5 * (tt)^2) / tau_m^2)
#
# #ff = f/sum(f)
# plot(tt,f)
