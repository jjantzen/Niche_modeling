doitall2 <- function (inname = "DoItAll", env1, env2, sp1, sp2, rarefy.dist = 0, 
                      rarefy.units = "dd", env.reso, reduce.env = 0, reductype = "PCA", 
          non.analogous.environments = "YES", nae.window = 5, env.trim = T, 
          env.trim.type = "MCP", trim.mask1, trim.mask2, trim.buffer.sp1 = 200, 
          trim.buffer.sp2 = 200, color.ramp = 1, correct.env = T, 
          pcx = 1, pcy = 2, col.env = e.var, e.var, R = 100, kern.smooth = 1, 
          e.reps = 100, b.reps = 100, b.force.equal.sample = F, nae = "YES", 
          thresh.espace.z = 0.001, p.overlap = T, p.boxplot = F, p.scatter = F, 
          run.silent = F, ncores = 1) 
{
  l <- list()
  colr.o <- humboldt.color(col = color.ramp)
  cores.n <- ncores
  if (ncores == "ALL") {
    ncores = "All"
  }
  if (ncores == "all") {
    ncores = "All"
  }
  zz <<- humboldt.g2e(env1, env2, sp1, sp2, reduce.env, rarefy.dist = 0, 
                      rarefy.units = "dd", reductype, 
                      non.analogous.environments, nae.window, env.trim, e.var, 
                      col.env = e.var, env.trim.type, trim.mask1, trim.mask2, 
                      trim.buffer.sp1, trim.buffer.sp2, pcx = pcx, pcy = pcy, 
                      env.reso, kern.smooth, R, 
                      run.silent = run.silent)
  ptlss1 <- nrow(zz$scores.sp1)/nrow(zz$scores.sp1FULL)
  ptlss2 <- nrow(zz$scores.sp2)/nrow(zz$scores.sp2FULL)
  if (reduce.env != 0 & ptlss1 > 0.5 & nrow(zz$scores.sp2) < 
      10) {
    print(paste("Only ", nrow(zz$scores.sp2), " points remain in species dataset 2. In this analysis, it is recommended to not trim shared e-space or remove non-analogous climates because input habitats are too divergent.  Basically input environments are non-equivalent and have little e-space overlap relevant to species 2. Because e-space cannot be reduced to shared/non-analogous environments: report shared analogos space percentage, niche similarity values and equivalence statistic significance for full climate space (see below for correct.env parameter recommendations)"))
  }
  if (reduce.env != 0 & ptlss2 > 0.5 & nrow(zz$scores.sp1) < 
      10) {
    print(paste("Only ", nrow(zz$scores.sp1), " points remain in species dataset 1. In this analysis, it is recommended to not trim shared e-space or remove non-analogous climates because input habitats are too divergent.  Basically input environments are non-equivalent and have little e-space overlap relevant to species 1. Because e-space cannot be reduced to shared/non-analogous environments: report shared analogos space percentage, niche similarity values and equivalence statistic significance for full climate space (see below for correct.env parameter recommendations)"))
  }
  threshinZ <- thresh.espace.z
  nacinZ <- nae
  kern.smoothinZ <- kern.smooth
  Rin <- R
  scores.env12 <- rbind(zz$scores.env1, zz$scores.env2)
  scores.env1a <- zz$scores.env1
  scores.env2a <- zz$scores.env2
  scores.env12a <- scores.env12
  scores.sp1a <- zz$scores.sp1
  scores.sp2a <- zz$scores.sp2
  z1 <- humboldt.grid.espace(scores.env12a[1:2], scores.env1a[1:2], 
                             scores.env1a[1:2], kern.smooth = kern.smoothinZ, R = Rin)
  z2 <- humboldt.grid.espace(scores.env12a[1:2], scores.env2a[1:2], 
                             scores.env2a[1:2], kern.smooth = kern.smoothinZ, R = Rin)
  z3 <- humboldt.grid.espace(scores.env1a[1:2], scores.env1a[1:2], 
                             scores.env1a[1:2], kern.smooth = kern.smoothinZ, R = Rin)
  z4 <- humboldt.grid.espace(scores.env2a[1:2], scores.env2a[1:2], 
                             scores.env2a[1:2], kern.smooth = kern.smoothinZ, R = Rin)
  z5 <- humboldt.grid.espace(scores.env12a[1:2], scores.env1a[1:2], 
                             scores.sp1a[1:2], kern.smooth = kern.smoothinZ, R = Rin)
  z6 <- humboldt.grid.espace(scores.env12a[1:2], scores.env2a[1:2], 
                             scores.sp2a[1:2], kern.smooth = kern.smoothinZ, R = Rin)
  DvalClim <- round(as.numeric(humboldt.niche.similarity(z3, 
                                                         z4, correct.env = F, nae = "YES", thresh.espace.z = threshinZ)[1]), 
                    3)
  pdf(file = paste(inname, "_ENV.pdf", sep = ""))
  layout(matrix(c(1, 1, 2, 2, 1, 1, 2, 2, 3, 3, 4, 4, 3, 3, 
                  4, 4), 4, 4, byrow = TRUE))
  humboldt.plot.niche(z1, correct.env = F, title = "E-space Environment 1", 
                      color.ramp = color.ramp, name.axis1 = paste("PC", pcx, 
                                                                  sep = ""), name.axis2 = paste("PC", pcy, sep = ""))
  humboldt.plot.niche(z2, correct.env = F, title = "E-space Environment 2", 
                      color.ramp = color.ramp, name.axis1 = paste("PC", pcx, 
                                                                  sep = ""), name.axis2 = paste("PC", pcy, sep = ""))
  ee <- humboldt.espace.correction(Z.env1 = z1, Z.env2 = z2, 
                                   Z.sp1 = z5, Z.sp2 = z6)
  if (correct.env == T) {
    print("Just so you know, you input: 'correct.env=T'")
  }
  humboldt.plot.espace.diff(espace.diff = ee, correct.env = F, 
                            pcx = pcx, pcy = pcy, type = "environment")
  if (ee$e.uncor.sum != 0) {
    contour(z1$x, (sort((z1$y))), z1$Z, add = T, levels = quantile(z1$Z[z1$Z > 
                                                                          0], c(0.1, 0.5, 0.75)), drawlabels = F, lty = c(1, 
                                                                                                                          2, 3), lwd = c(1, 1, 1), col = "grey")
  }
  humboldt.plot.contrib(zz$pca.cal$co, pcx = pcx, pcy = pcy, 
                        zz$pca.cal$eig)
  dev.off()
  pdf(file = paste(inname, "_ENV2.pdf", sep = ""))
  layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
  AmaxF <- max(zz$scores.env1FULL[, 1])
  AminF <- min(zz$scores.env1FULL[, 1])
  BmaxF <- max(zz$scores.env2FULL[, 1])
  BminF <- min(zz$scores.env2FULL[, 1])
  CmaxF <- max(zz$scores.env1FULL[, 2])
  CminF <- min(zz$scores.env1FULL[, 2])
  DmaxF <- max(zz$scores.env2FULL[, 2])
  DminF <- min(zz$scores.env2FULL[, 2])
  Xmin1 = pmin(AminF, BminF)
  Xmax1 = pmax(AmaxF, BmaxF)
  Ymin1 = pmin(CminF, DminF)
  Ymax1 = pmax(CmaxF, DmaxF)
  AmaxFG <- max(zz$scores.env1FULL[, 3])
  AminFG <- min(zz$scores.env1FULL[, 3])
  BmaxFG <- max(zz$scores.env2FULL[, 3])
  BminFG <- min(zz$scores.env2FULL[, 3])
  CmaxFG <- max(zz$scores.env1FULL[, 4])
  CminFG <- min(zz$scores.env1FULL[, 4])
  DmaxFG <- max(zz$scores.env2FULL[, 4])
  DminFG <- min(zz$scores.env2FULL[, 4])
  plot(zz$scores.env1FULL[1:2], col = "light grey", xlim = c(Xmin1, 
                                                             Xmax1), ylim = c(Ymin1, Ymax1), main = "E-space Environment 1", 
       xlab = paste("PC", pcx, sep = ""), ylab = paste("PC", 
                                                       pcy, sep = ""), pch = 20, cex.sub = 0.75, cex = 0.3, 
       sub = (paste("Removed:", zz$RedP1C, "sites of", nrow(zz$scores.env1FULL[1:2]), 
                    "(", round((100 * zz$RedP1C/nrow(zz$scores.env1FULL[1:2])), 
                               1), "%);", zz$RedP1sp, "locs of", nrow(zz$scores.sp1FULL[1:2]), 
                    "(", round((100 * zz$RedP1sp/nrow(zz$scores.sp1FULL[1:2])), 
                               1), "%)")))
  points(zz$scores.env1[1:2], pch = 19, cex = 0.3)
  points(zz$scores.sp1FULL[1:2], pch = 19, col = "blue", cex = 0.3)
  points(zz$scores.sp1[1:2], pch = 19, col = "red", cex = 0.3)
  rect(min(zz$scores.env2[, 1], zz$scores.env1[, 1]), min(zz$scores.env2[, 
                                                                         2], zz$scores.env1[, 2]), max(zz$scores.env2[, 1], zz$scores.env1[, 
                                                                                                                                           1]), max(zz$scores.env2[, 2], zz$scores.env1[, 2]), 
       density = NULL, angle = 45, col = NA, border = NULL, 
       lty = par("lty"), lwd = par("lwd"))
  plot(zz$scores.env2FULL[1:2], col = "light grey", xlim = c(Xmin1, 
                                                             Xmax1), ylim = c(Ymin1, Ymax1), main = "E-space Environment 2", 
       xlab = paste("PC", pcx, sep = ""), ylab = paste("PC", 
                                                       pcy, sep = ""), pch = 20, cex.sub = 0.75, cex = 0.3, 
       sub = (paste("Removed:", zz$RedP2C, "sites of", nrow(zz$scores.env2FULL[1:2]), 
                    "(", round((100 * zz$RedP2C/nrow(zz$scores.env2FULL[1:2])), 
                               1), "%);", zz$RedP2sp, "locs of", nrow(zz$scores.sp2FULL[1:2]), 
                    "(", round((100 * zz$RedP2sp/nrow(zz$scores.sp2FULL[1:2])), 
                               1), "%)")))
  points(zz$scores.env2[1:2], pch = 19, cex = 0.3)
  points(zz$scores.sp2FULL[1:2], pch = 19, col = "blue", cex = 0.3)
  points(zz$scores.sp2[1:2], pch = 19, col = "red", cex = 0.3)
  rect(min(zz$scores.env2[, 1], zz$scores.env1[, 1]), min(zz$scores.env2[, 
                                                                         2], zz$scores.env1[, 2]), max(zz$scores.env2[, 1], zz$scores.env1[, 
                                                                                                                                           1]), max(zz$scores.env2[, 2], zz$scores.env1[, 2]), 
       density = NULL, angle = 45, col = NA, border = NULL, 
       lty = par("lty"), lwd = par("lwd"))
  plot(zz$scores.env1FULL[3:4], col = "light grey", xlim = c(AminFG, 
                                                             AmaxFG), ylim = c(CminFG, CmaxFG), main = "G-space Environment 1", 
       xlab = "Longitude", ylab = "Latitude", pch = 20, cex.sub = 0.75, 
       cex = 0.3, sub = (paste("Removed:", zz$RedP1C, "sites of", 
                               nrow(zz$scores.env1FULL[3:4]), "(", round((100 * 
                                                                            zz$RedP1C/nrow(zz$scores.env1FULL[3:4])), 1), 
                               "%);", zz$RedP1sp, "locs of", nrow(zz$scores.sp1FULL[3:4]), 
                               "(", round((100 * zz$RedP1sp/nrow(zz$scores.sp1FULL[3:4])), 
                                          1), "%)")))
  points(zz$scores.env1[3:4], pch = 19, cex = 0.3)
  points(zz$scores.sp1FULL[3:4], pch = 19, col = "blue", cex = 0.3)
  points(zz$scores.sp1[3:4], pch = 19, col = "red", cex = 0.3)
  plot(zz$scores.env2FULL[3:4], col = "light grey", xlim = c(BminFG, 
                                                             BmaxFG), ylim = c(DminFG, DmaxFG), main = "G-space Environment 2", 
       xlab = "Longitude", ylab = "Latitude", pch = 20, cex.sub = 0.75, 
       cex = 0.3, sub = (paste("Removed:", zz$RedP2C, "sites of", 
                               nrow(zz$scores.env2FULL[3:4]), "(", round((100 * 
                                                                            zz$RedP2C/nrow(zz$scores.env2FULL[3:4])), 1), 
                               "%);", zz$RedP2sp, "locs of", nrow(zz$scores.sp2FULL[3:4]), 
                               "(", round((100 * zz$RedP2sp/nrow(zz$scores.sp2FULL[3:4])), 
                                          1), "%)")))
  points(zz$scores.env2[3:4], pch = 19, cex = 0.3)
  points(zz$scores.sp2FULL[3:4], pch = 19, col = "blue", cex = 0.3)
  points(zz$scores.sp2[3:4], pch = 19, col = "red", cex = 0.3)
  dev.off()
  pdf(file = paste(inname, "_ENV3.pdf", sep = ""))
  layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
  par(mar = c(2.5, 4, 2.5, 2.5))
  options(warn = -1)
  n.env1 <- 1:nrow(zz$scores.env1)
  n.env2 <- (nrow(zz$scores.env1) + 1):(nrow(zz$scores.env1) + 
                                          nrow(zz$scores.env2))
  env12 <- rbind(zz$scores.env1, zz$scores.env2)
  Z1 <- env12[, 1]
  Z2 <- env12[, 2]
  X <- env12[, 3]
  Y <- env12[, 4]
  df <- data.frame(X, Y, Z1, Z2)
  rangeMin <- min(df[, 3])
  rangeMax <- max(df[, 3])
  rangeEnv <- (rangeMax - rangeMin)/256
  rangeEnvL <- (rangeMax - rangeMin)
  rangeBreaks <- seq(rangeMin, rangeMax, rangeEnv)
  r.range <- c(rangeMin, rangeMax)
  div.r.range <- rangeEnvL/5
  df <- df[n.env1, ]
  r3 <- raster(res = env.reso, xmn = min(df[, 1]), xmx = max(df[, 
                                                                1]), ymn = min(df[, 2]), ymx = max(df[, 2]))
  cols <- colr.o
  cells <- cellFromXY(r3, df[, 1:2])
  r3[cells] <- df[, 3]
  plot(r3, col = cols, cex.axis = 0.6, legend = FALSE, breaks = rangeBreaks)
  plot(r3, legend.only = TRUE, col = cols, legend.width = 1, 
       legend.shrink = 0.5, axis.args = list(at = seq(r.range[1], 
                                                      r.range[2], div.r.range), labels = round(seq(r.range[1], 
                                                                                                   r.range[2], div.r.range), 1), cex.axis = 0.6), legend.args = list(text = paste("PC", 
                                                                                                                                                                                  pcx, sep = ""), side = 4, font = 2, line = 2.5, 
                                                                                                                                                                     cex = 0.8))
  df <- data.frame(X, Y, Z1, Z2)
  df <- df[n.env2, ]
  r4 <- raster(res = env.reso, xmn = min(df[, 1]), xmx = max(df[, 
                                                                1]), ymn = min(df[, 2]), ymx = max(df[, 2]))
  cols <- colr.o
  cells <- cellFromXY(r4, df[, 1:2])
  r4[cells] <- df[, 3]
  plot(r4, col = cols, cex.axis = 0.6, legend = FALSE, breaks = rangeBreaks)
  df <- data.frame(X, Y, Z1, Z2)
  rangeMin <- min(df[, 4])
  rangeMax <- max(df[, 4])
  rangeEnv <- (rangeMax - rangeMin)/256
  rangeEnvL <- (rangeMax - rangeMin)
  rangeBreaks <- seq(rangeMin, rangeMax, rangeEnv)
  r.range <- c(rangeMin, rangeMax)
  div.r.range <- rangeEnvL/5
  df <- df[n.env1, ]
  r1 <- raster(res = env.reso, xmn = min(df[, 1]), xmx = max(df[, 
                                                                1]), ymn = min(df[, 2]), ymx = max(df[, 2]))
  cols <- colr.o
  cells <- cellFromXY(r1, df[, 1:2])
  r1[cells] <- df[, 4]
  plot(r1, col = cols, cex.axis = 0.6, legend = FALSE, breaks = rangeBreaks)
  plot(r1, legend.only = TRUE, col = cols, legend.width = 1, 
       legend.shrink = 0.5, axis.args = list(at = seq(r.range[1], 
                                                      r.range[2], div.r.range), labels = round(seq(r.range[1], 
                                                                                                   r.range[2], div.r.range), 1), cex.axis = 0.6), legend.args = list(text = paste("PC", 
                                                                                                                                                                                  pcy, sep = ""), side = 4, font = 2, line = 2.5, 
                                                                                                                                                                     cex = 0.8))
  df <- data.frame(X, Y, Z1, Z2)
  df <- df[n.env2, ]
  r2 <- raster(res = env.reso, xmn = min(df[, 1]), xmx = max(df[, 
                                                                1]), ymn = min(df[, 2]), ymx = max(df[, 2]))
  cols <- colr.o
  cells <- cellFromXY(r2, df[, 1:2])
  r2[cells] <- df[, 4]
  plot(r2, col = cols, cex.axis = 0.6, legend = FALSE, breaks = rangeBreaks)
  mtext("E-space plotted in G-space", side = 3, line = -2, 
        outer = TRUE)
  options(warn = 0)
  dev.off()
  print("testing")
  a <- equiv(z5, z6, rep = e.reps, correct.env = correct.env, 
                                 nae = nacinZ, kern.smooth = kern.smoothinZ, run.silent.equ = run.silent, 
                                 ncores = cores.n) #humboldt.equivalence.stat
  print("testing2")
  b <- humboldt.background.stat(g2e = zz, correct.env = correct.env, 
                                env.reso = env.reso, sim.dir = 1, rep = b.reps, kern.smooth = kern.smoothinZ, 
                                R = Rin, force.equal.sampl = b.force.equal.sample, run.silent.bak = run.silent, 
                                ncores = cores.n)
  print("testing3")
  b2 <- humboldt.background.stat(g2e = zz, correct.env = correct.env, 
                                 env.reso = env.reso, sim.dir = 2, rep = b.reps, kern.smooth = kern.smoothinZ, 
                                 R = Rin, force.equal.sampl = b.force.equal.sample, run.silent.bak = run.silent, 
                                 ncores = cores.n)
  print("****************")
  
  print("Species 1:")
  pnt1 <- humboldt.pnt.index(scores.env12a[1:2], scores.env1a[1:2], 
                             scores.sp1a[1:2], kern.smooth = kern.smoothinZ, R = Rin)
  print("Species 2:")
  pnt2 <- humboldt.pnt.index(scores.env12a[1:2], scores.env2a[1:2], 
                             scores.sp2a[1:2], kern.smooth = kern.smoothinZ, R = Rin)
  print("Measuring Niche Similarity:")
  pnt1R <- round(pnt1$pnt.index, d = 2)
  pnt2R <- round(pnt2$pnt.index, d = 2)
  if (pnt1R > 1) {
    pnt1R <- 1
  }
  if (pnt1R < 0.05) {
    pnt1R <- paste("  PNT.index: ", pnt1R, "; minimm PNT", 
                   sep = "")
  }
  if (pnt1R >= 0.05 & pnt1R < 0.15) {
    pnt1R <- paste("  PNT.index: ", pnt1R, "; some PNT", 
                   sep = "")
  }
  if (pnt1R >= 0.15 & pnt1R < 0.3) {
    pnt1R <- paste("  PNT.index: ", pnt1R, "; moderate PNT", 
                   sep = "")
  }
  if (pnt1R >= 0.3) {
    pnt1R <- paste("  PNT.index: ", pnt1R, "; high PNT", 
                   sep = "")
  }
  if (pnt2R > 1) {
    pnt2R <- 1
  }
  if (pnt2R < 0.05) {
    pnt2R <- paste("  PNT.index: ", pnt2R, "; minimm PNT", 
                   sep = "")
  }
  if (pnt2R >= 0.05 & pnt2R < 0.15) {
    pnt2R <- paste("  PNT.index: ", pnt2R, "; some PNT", 
                   sep = "")
  }
  if (pnt2R >= 0.15 & pnt2R < 0.3) {
    pnt2R <- paste("  PNT.index: ", pnt2R, "; moderate PNT", 
                   sep = "")
  }
  if (pnt2R >= 0.3) {
    pnt2R <- paste("  PNT.index: ", pnt2R, "; high PNT", 
                   sep = "")
  }
  pdf(file = paste(inname, "_SP.pdf", sep = ""))
  layout(matrix(c(1, 1, 2, 2, 1, 1, 2, 2, 3, 3, 4, 5, 3, 3, 
                  6, 7), 4, 4, byrow = TRUE))
  humboldt.plot.niche(z5, title = "E-space Species 1", color.ramp = color.ramp, 
                      name.axis1 = paste("PC", pcx, ", ", pnt1R, sep = ""), 
                      name.axis2 = paste("PC", pcy, sep = ""), correct.env = F)
  humboldt.plot.niche(z6, title = "E-space Species 2", color.ramp = color.ramp, 
                      name.axis1 = paste("PC", pcx, ", ", pnt2R, sep = ""), 
                      name.axis2 = paste("PC", pcy, sep = ""), correct.env = F)
  humboldt.plot.espace.diff(espace.diff = ee, correct.env = F, 
                            type = "species", pcx = pcx, pcy = pcy)
  if (ee$s.uncor.sum != 0) {
    contour(z5$x, (sort((z5$y))), z5$Z, add = T, levels = quantile(z5$Z[z5$Z > 
                                                                          0], c(0.1, 0.5, 0.75)), drawlabels = F, lty = c(1, 
                                                                                                                          2, 3), lwd = c(1, 1, 1), col = "grey")
  }
  if (ee$s.uncor.sum == 0) {
    contour(z5$x, (sort((z5$y))), z5$Z, add = T, levels = quantile(z5$Z[z5$Z > 
                                                                          0], c(0.1, 0.5, 0.75)), drawlabels = F, lty = c(1, 
                                                                                                                          2, 3), lwd = c(1, 1, 1), col = "white")
  }
  DvalSp <- round(as.numeric(humboldt.niche.similarity(z5, 
                                                       z6, correct.env = correct.env, nae = "YES", thresh.espace.z = threshinZ)[1]), 
                  3)
  DvalSp2 <- round(as.numeric(humboldt.niche.similarity(z5, 
                                                        z6, correct.env = correct.env, nae = "NO", thresh.espace.z = threshinZ)[1]), 
                   3)
  if (DvalSp2 < DvalSp) {
    DvalSp2 <- DvalSp
  }
  DvalAdjSp <- round((DvalSp/DvalClim), 3)
  DvalAdjSp[DvalAdjSp > 1] <- 1
  print("Similarity:")
  print(DvalSp)
  print("Similarity - analogous environments only:")
  print(DvalSp2)
  plot.new()
  text(0.5, 0.5, paste("Niche Similarity:", "\n", "D=", DvalSp, 
                       "\n", "Analog Env ONLY:", "\n", "D=", DvalSp2), cex = 1)
  humboldt.plot.histrogram(a, "D", "Equivalency")
  humboldt.plot.density(b, "D", "Background 2->1")
  humboldt.plot.density(b2, "D", "Background 1->2")
  dev.off()
  print("FINISHED ANALYSES: now generating supplemental plots (if selected)")
  if (p.scatter == T) {
    pdf(file = paste(inname, "_SCATTER.pdf", sep = ""))
    humboldt.plot.scatter(zz$scores.env1[1:2], xlab = paste("PC", 
                                                            pcx, sep = ""), ylab = paste("PC", pcy, sep = ""), 
                          main = "Environment 1 PCA", color.ramp = color.ramp)
    humboldt.plot.scatter(zz$scores.env2[1:2], xlab = paste("PC", 
                                                            pcx, sep = ""), ylab = paste("PC", pcy, sep = ""), 
                          main = "Environment 2 PCA", color.ramp = color.ramp)
    humboldt.plot.scatter(zz$scores.sp1[1:2], xlab = paste("PC", 
                                                           pcx, sep = ""), ylab = paste("PC", pcy, sep = ""), 
                          main = "Sp/pop 1 PCA", color.ramp = color.ramp)
    humboldt.plot.scatter(zz$scores.sp2[1:2], xlab = paste("PC", 
                                                           pcx, sep = ""), ylab = paste("PC", pcy, sep = ""), 
                          main = "Sp/pop 2 PCA", color.ramp = color.ramp)
    dev.off()
  }
  if (p.boxplot == T) {
    AmaxPCA1 <- max(zz$scores.env1[, 1])
    AminPCA1 <- min(zz$scores.env1[, 1])
    AmeanPCA1 <- mean(zz$scores.env1[, 1])
    BmaxPCA1 <- max(zz$scores.env2[, 1])
    BminPCA1 <- min(zz$scores.env2[, 1])
    BmeanPCA1 <- mean(zz$scores.env2[, 1])
    XminPCA1 = pmin(AmaxPCA1, BmaxPCA1)
    XmaxPCA1 = pmax(AminPCA1, BminPCA1)
    AmaxPCA2 <- max(zz$scores.env1[, 2])
    AminPCA2 <- min(zz$scores.env1[, 2])
    AmeanPCA2 <- mean(zz$scores.env1[, 2])
    BmaxPCA2 <- max(zz$scores.env2[, 2])
    BminPCA2 <- min(zz$scores.env2[, 2])
    BmeanPCA2 <- mean(zz$scores.env2[, 2])
    XminPCA2 = pmin(AmaxPCA2, BmaxPCA2)
    XmaxPCA2 = pmax(AminPCA2, BminPCA2)
    CmaxPCA1 <- max(zz$scores.sp1[, 1])
    CminPCA1 <- min(zz$scores.sp1[, 1])
    CmeanPCA1 <- mean(zz$scores.sp1[, 1])
    DmaxPCA1 <- max(zz$scores.sp2[, 1])
    DminPCA1 <- min(zz$scores.sp2[, 1])
    DmeanPCA1 <- mean(zz$scores.sp2[, 1])
    CmaxPCA2 <- max(zz$scores.sp1[, 2])
    CminPCA2 <- min(zz$scores.sp1[, 2])
    CmeanPCA2 <- mean(zz$scores.sp1[, 2])
    DmaxPCA2 <- max(zz$scores.sp2[, 2])
    DminPCA2 <- min(zz$scores.sp2[, 2])
    DmeanPCA2 <- mean(zz$scores.sp2[, 2])
    EmaxPCA1 <- max(zz$scores.sp1FULL[, 1])
    EminPCA1 <- min(zz$scores.sp1FULL[, 1])
    EmeanPCA1 <- mean(zz$scores.sp1FULL[, 1])
    FmaxPCA1 <- max(zz$scores.sp2FULL[, 1])
    FminPCA1 <- min(zz$scores.sp2FULL[, 1])
    FmeanPCA1 <- mean(zz$scores.sp2FULL[, 1])
    EmaxPCA2 <- max(zz$scores.sp1FULL[, 2])
    EminPCA2 <- min(zz$scores.sp1FULL[, 2])
    EmeanPCA2 <- mean(zz$scores.sp1FULL[, 2])
    FmaxPCA2 <- max(zz$scores.sp2FULL[, 2])
    FminPCA2 <- min(zz$scores.sp2FULL[, 2])
    FmeanPCA2 <- mean(zz$scores.sp2FULL[, 2])
    pdf(file = paste(inname, "_BOXPLOT.pdf", sep = ""))
    layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE))
    X1 = c(AminF, EminPCA1, EmeanPCA1, EmaxPCA1, AmaxF)
    Y1 = c(BminF, FminPCA1, FmeanPCA1, FmaxPCA1, BmaxF)
    XX1 = c(AminPCA1, CminPCA1, CmeanPCA1, CmaxPCA1, AmaxPCA1)
    YY1 = c(BminPCA1, DminPCA1, DmeanPCA1, DmaxPCA1, BmaxPCA1)
    df = data.frame(X1, Y1, XX1, YY1)
    bp = boxplot(df, horizontal = TRUE, axes = TRUE, staplewex = 1, 
                 lwd = 1, whisklty = 1, medlwd = 0, medcol = c("olivedrab3", 
                                                               "skyblue2", "olivedrab3", "skyblue2"), col = c("olivedrab3", 
                                                                                                              "skyblue2", "olivedrab3", "skyblue2"), range = 0, 
                 xlab = paste("environmental space- PC", pcx, sep = ""), 
                 at = c(5, 4, 2, 1), names = c("sp/pop1", "sp/pop2", 
                                               "overlap\nsp/pop1", "overlap\nsp/pop2"), las = 1)
    abline(v = AmaxPCA1, lty = 2)
    abline(v = AminPCA1, lty = 2)
    aZ = data.frame(zz$scores.sp1FULL[, 1], "sp1F")
    bZ = data.frame(zz$scores.sp2FULL[, 1], "sp2F")
    aF = data.frame(zz$scores.sp1[, 1], "sp1")
    bF = data.frame(zz$scores.sp2[, 1], "sp2")
    names(aF)[1] <- "Val"
    names(aZ)[1] <- "Val"
    names(bF)[1] <- "Val"
    names(bZ)[1] <- "Val"
    names(aF)[2] <- "G"
    names(aZ)[2] <- "G"
    names(bF)[2] <- "G"
    names(bZ)[2] <- "G"
    df2 = rbind(aZ, bZ, aF, bF)
    stripchart(Val ~ G, vertical = FALSE, data = df2, method = "jitter", 
               jitter = 0.3, add = TRUE, pch = 20, col = "black", 
               at = c(5, 4, 2, 1))
    X1 = c(CminF, EminPCA2, EmeanPCA2, EmaxPCA2, CmaxF)
    Y1 = c(DminF, FminPCA2, FmeanPCA2, FmaxPCA2, DmaxF)
    XX1 = c(AminPCA2, CminPCA2, CmeanPCA2, CmaxPCA2, AmaxPCA2)
    YY1 = c(BminPCA2, DminPCA2, DmeanPCA2, DmaxPCA2, BmaxPCA2)
    df3 = data.frame(X1, Y1, XX1, YY1)
    bp2 = boxplot(df3, horizontal = TRUE, axes = TRUE, staplewex = 1, 
                  lwd = 1, whisklty = 1, medlwd = 0, medcol = c("olivedrab3", 
                                                                "skyblue2", "olivedrab3", "skyblue2"), col = c("olivedrab3", 
                                                                                                               "skyblue2", "olivedrab3", "skyblue2"), range = 0, 
                  xlab = paste("environmental space- PC", pcy, sep = ""), 
                  at = c(5, 4, 2, 1), names = c("sp/pop1", "sp/pop2", 
                                                "overlap\nsp/pop1", "overlap\nsp/pop2"), las = 1)
    abline(v = AminPCA2, lty = 2)
    abline(v = AmaxPCA2, lty = 2)
    aZ = data.frame(zz$scores.sp1FULL[, 2], "sp1F")
    bZ = data.frame(zz$scores.sp2FULL[, 2], "sp2F")
    aF = data.frame(zz$scores.sp1[, 2], "sp1")
    bF = data.frame(zz$scores.sp2[, 2], "sp2")
    names(aF)[1] <- "Val"
    names(aZ)[1] <- "Val"
    names(bF)[1] <- "Val"
    names(bZ)[1] <- "Val"
    names(aF)[2] <- "G"
    names(aZ)[2] <- "G"
    names(bF)[2] <- "G"
    names(bZ)[2] <- "G"
    df4 = rbind(aZ, bZ, aF, bF)
    stripchart(Val ~ G, vertical = FALSE, data = df4, method = "jitter", 
               jitter = 0.3, add = TRUE, pch = 20, col = "black", 
               at = c(5, 4, 2, 1))
    dev.off()
  }
  if (p.overlap == T) {
    humboldt.plot.overlap(in.g2e = zz, pdf.out = T, pcx = pcx, 
                          pcy = pcy, pdfname = paste(inname, "-SP-OVERLAP.pdf", 
                                                     sep = ""))
  }
  l$g2e <- zz
  l$eqiv <- a
  l$b21 <- b
  l$b12 <- b2
  invisible(l)
}
