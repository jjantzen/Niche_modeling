equiv <- function (z1, z2, rep = 100, correct.env = T, kern.smooth = 1, 
          nae = "YES", thresh.espace.z = 0.001, run.silent.equ = F, 
          ncores = 1) 
{
  if (ncores == "ALL") {
    ncores = "All"
  }
  if (ncores == "all") {
    ncores = "All"
  }
  if (nae == "yes") {
    ncores = "YES"
  }
  if (nae == "Yes") {
    ncores = "YES"
  }
  if (nae == "no") {
    ncores = "NO"
  }
  if (nae == "No") {
    ncores = "NO"
  }
  if (kern.smooth == "AUTO") {
    kern.smooth = "auto"
  }
  if (kern.smooth == "Auto") {
    kern.smooth = "auto"
  }
  threshinZ <- thresh.espace.z
  kern.smoothinZ <- kern.smooth
  repT = rep
  nacinZ <- nae
  Rin <- length(z1$x)
  cor.env.n <- correct.env
  pb <- NULL
  switch(Sys.info()[["sysname"]], Windows = {
    userOS = 1
  }, Linux = {
    userOS = 2
  }, Darwin = {
    userOS = 2
  })
  tCores <- detectCores()
  if (ncores == "ALL") {
    ncores <- (tCores - 1)
  }
  if (tCores == ncores & tCores > 1) {
    ncores <- (tCores - 1)
  }
  if (tCores < ncores) {
    ncores <- (tCores - 1)
  }
  l <- list()
  obs.o <- humboldt.niche.similarity(z1, z2, correct.env = cor.env.n, 
                                     nae = nacinZ, thresh.espace.z = threshinZ)
  if (ncores == 1) {
    if (run.silent.equ == F & userOS == 1) {
      pb <- winProgressBar(title = "Initializing", min = 0, 
                           max = repT, width = 300)
    }
    if (run.silent.equ == F & userOS == 2) {
      pb <- tkProgressBar(title = "Initializing", label = "", 
                          min = 0, max = repT, initial = 0, width = 300)
    }
    sim.o <- as.data.frame(matrix(unlist(lapply(1:rep, equivalency.iter, 
                                                z1, z2, nacinZ, threshinZ, kern.smoothinZ, Rin, 
                                                pb, repT, ncores, run.silent.equ, cor.env.n, userOS)), 
                                  byrow = TRUE, ncol = 2))
  }
  if (ncores > 1) {
    cl <- makeCluster(ncores)
    clusterExport(cl = cl, c("z1", "z2"), envir = environment())
    invisible(clusterEvalQ(cl, library(humboldt)))
    sim.o <- as.data.frame(matrix(unlist(pblapply(1:rep, 
                                                  equivalency.iter, z1, z2, nacinZ, threshinZ, kern.smoothinZ, 
                                                  Rin, pb, repT, ncores, run.silent.equ, cor.env.n, 
                                                  userOS, cl = cl)), byrow = TRUE, ncol = 2))
    stopCluster(cl)
  }
  colnames(sim.o) <- c("D", "I")
  l$sim <- sim.o
  l$obs <- obs.o
  if (ncores == 1 & run.silent.equ == F) {
    close(pb)
  }
  l$sim <- sim.o
  l$obs$D <- obs.o$D
  l$obs$I <- obs.o$I
  p.D <- (sum(sim.o$D <= obs.o$D) + 1)/(length(sim.o$D) + 
                                          1)
  l$p.D <- p.D
  l$p.I <- (sum(sim.o$I <= obs.o$I) + 1)/(length(sim.o$I) + 
                                            1)
  return(l)
}
