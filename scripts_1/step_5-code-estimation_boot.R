rm(list = ls(all = T))
devtools::load_all() # meu pacote
devtools::install()
#devtools::document()

require(snowfall)
cpus <- 2
MC <- 200
n <- 1000
B <- 1
mtrim <- .20
sfInit(parallel = TRUE, cpus = cpus)#slaveOutfile = "j.txt"


#sfLibrary(snowfall)
sfExportAll()
sfLibrary(thesiR)
sfLibrary(tidyverse)
sfLibrary(tictoc)
sfLibrary(extraDistr)
sfLibrary(rgdal)
tic<-tictoc::tic()
#a <- sfLapply(1:MC,fun=boot_kumar)
a <- sfClusterApplyLB(
  1:MC,
  fun = boot_kumar_no_par,
  B = B,
  n = n,
  mtrim = mtrim
)
toc<-tictoc::toc()
sfStop()
#27147.89 sec elapsed mc = 100 B=1
# 27147.89+25752.06 sec elapsed mc=201 B=1
#39752.9 sec elapsed mc=100 B=20
#3920.446 sec elapsed não paramétrico MC=200
#3920.446 + 2914.398  sec elapsed não paramétrico MC=400
