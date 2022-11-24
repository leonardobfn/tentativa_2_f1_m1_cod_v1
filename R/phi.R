phi <- function(zm, alfastart, BB, TT = TT, Lmm = Lmm) {
  # BB=B;zm=z
  #n=50000
  #p0=p1=p2=NULL
  #for(i in 1:200){
    #zm <-rgamma(n,2,9)
    #BB <- runif(n,0,1)
    #alfastart <- 0.25
    CB <- ((sin(pi * alfastart * BB) / sin(pi * BB))^(1 / (1 - alfastart))) * ((sin(pi * (1 - alfastart) * BB)) / sin(pi * alfastart * BB))
    # print(CB)
    aux_CB <- CB * exp(-CB * zm^(-alfastart / (1 - alfastart)))

    # aux_CB[aux_CB==0] <- 1

    phi0 <- mean(aux_CB, na.rm = F)
    phi1 <- mean(log(CB) * aux_CB, na.rm = F)
    phi2 <- mean(CB * aux_CB, na.rm = F)
    #p0=append(p0,phi0)
    #p1=append(p1,phi1)
    #p2=append(p2,phi2)
    #p0;p1;p2
  #}
  # phiB <- mean(BB*aux_CB)
  return(data.frame(phi0, phi1, phi2))
}
