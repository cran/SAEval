
cinterval<-function(data, dir, sae, v.dir, mse.sae, level = 0.95, plot = F) 
{
  dir.name <- all.vars(dir)
  v.dir.name <- all.vars(v.dir)
  sae.name <- all.vars(sae)
  mse.sae.name <- all.vars(mse.sae)
  data.s <- data[, c(dir.name, v.dir.name, sae.name, mse.sae.name)]
  estimates2 <- data.s[!is.na(data.s[, v.dir.name]) & data.s[,v.dir.name] != 0, ]
  cov <- numeric(0)
  cov2 <- numeric(0)
  a <- 1 - level
  z <- qnorm(1 - a/2)
  yUP <- estimates2[, dir.name] + z * sqrt(estimates2[, v.dir.name])
  yLW <- estimates2[, dir.name] - z * sqrt(estimates2[, v.dir.name])
  for (i in 1:length(sae.name)) {
    xUP <- estimates2[, sae.name[i]] + z * sqrt(estimates2[,mse.sae.name[i]])
    xLW <- estimates2[, sae.name[i]] - z * sqrt(estimates2[,mse.sae.name[i]])
    coverage <- data.frame(y_d = estimates2[, dir.name], 
                           y_mod = estimates2[, sae.name[i]], xUP, xLW, yUP, 
                           yLW)
    # cov[i] <- sum(ifelse((coverage$y_mod > coverage$yLW) & 
    #                        (coverage$y_mod < coverage$yUP), 1, 0))
    cov[i] <- sum(ifelse((coverage$y_d > coverage$xLW) &
                           (coverage$y_d < coverage$xUP), 1, 0))
    
    cov2[i] <- sum(ifelse((coverage$yUP > coverage$xLW | 
                             coverage$xLW > coverage$yLW) & (coverage$yUP > coverage$xUP | 
                                                               coverage$xUP > coverage$yLW), 1, 0))
    if (plot != FALSE) {
      plot(1:dim(coverage)[1], coverage$y_d, type = "p", 
           xlab = "Domains", ylab = "", ylim = c(min(coverage$y_d, 
                                                     coverage$xLW), max(coverage$y_d, coverage$xUP)))
      polygon(c(1:dim(coverage)[1], rev(1:dim(coverage)[1])), 
              c(coverage$xLW, rev(coverage$xUP)), col = "grey75", 
              border = FALSE)
      points(1:dim(coverage)[1], coverage$y_d)
      lines(1:dim(coverage)[1], coverage$xUP, col = "red", 
            lty = 2)
      lines(1:dim(coverage)[1], coverage$xLW, col = "red", 
            lty = 2)
      legend("topleft", c(dir.name, paste("SAE CI ", 
                                          level, "%", sep = "")), lty = c(NA, 
                                                                          2), pch = c(1, NA), col = c("black", "red"), 
             cex = 0.8, bty = "n")
    }
  }
  output <- data.frame(methods = sae.name, included = cov, 
                       overlap = cov2)
  all.output <- data.frame(output)
}
