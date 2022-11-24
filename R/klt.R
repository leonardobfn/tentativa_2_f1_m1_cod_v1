klt <- function(eta_1, eta_2, BETA, ar.ma, p, q, link_kl = link_kl, y.d) {
  i <- max(p, q)
  ar <- seq(1:p)
  ma <- seq(1:q)
  eta_klt <- r <- rep(0, length(eta_1))
  # y.d=DADOS$y

  if (p != 0 & q != 0) { # Gkarma(p,q) model
    phi_ar <- ar.ma[1:p]
    phi_ma <- ar.ma[-(1:p)]
    for (t in (1 + i):length(eta_1)) {
      # eta_klt[t] <-  eta_1[t] +  phi_ar%*%eta_2[t-ar] + phi_ma%*%r[t-ma]
      eta_klt[t] <- eta_1[t] + phi_ar %*% eta_2[t - ar] + phi_ma %*% r[t - ma]
      r[t] <- link_kl$linkfun(y.d[t]) - eta_klt[t]
    }
  }

  if (p != 0 & q == 0) { # GKarma(p,0) model
    phi_ar <- ar.ma[1:p]
    for (t in (1 + i):length(eta_1)) {
      eta_klt[t] <- eta_1[t] + phi_ar %*% eta_2[t - ar]
      # eta_klt[t] <-  eta_1[t] + phi_ar%*%eta_2[t-ar]
      # r[t]<- link_kl$linkfun(y.d[t]) - eta_klt[t]
    }
  }

  if (p == 0 & q != 0) { # GKarma(0,q) model
    phi_ma <- ar.ma[(1:q)]
    for (t in (1 + i):length(eta_1)) {
      eta_klt[t] <- eta_1[t] + phi_ma %*% r[t - ma]
      # eta_klt[t] <- BETA[1] + phi_ma%*%r[t-ma] # code fabio
      r[t] <- link_kl$linkfun(y.d[t]) - eta_klt[t]
    }
  }



  klt <- eta_klt[eta_klt != 0]
  klt <- link_kl$linkinv(klt)
  return(klt)
}
