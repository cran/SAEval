
cinterval<-function (data, dir, sae, v.dir, mse.sae,level=0.95,plot=F) 
{
  dir.name <- all.vars(dir)
  v.dir.name <- all.vars(v.dir)
  sae.name <- all.vars(sae)
  mse.sae.name <- all.vars(mse.sae)
  data.s <- data[, c(dir.name, v.dir.name, sae.name, mse.sae.name)]
  estimates2 <- data.s[!is.na(data.s[, v.dir.name]) & data.s[,v.dir.name] != 0, ]
  
  cov<- numeric(0)
  
  a<-1-level
  z<-qnorm(1-a/2)
    
  yUP <- estimates2[, dir.name] + z * sqrt(estimates2[, v.dir.name])
  yLW <- estimates2[, dir.name] - z * sqrt(estimates2[, v.dir.name])
  
  for (i in 1:length(sae.name)) { 
    
    xUP <- estimates2[, sae.name[i]] + z * sqrt(estimates2[, mse.sae.name[i]])
    xLW <- estimates2[, sae.name[i]] - z * sqrt(estimates2[,  mse.sae.name[i]])

    coverage <- data.frame(y_d = estimates2[, dir.name], 
                           y_mod = estimates2[, sae.name[i]], xUP, xLW, yUP, 
                           yLW)
    cov[i]<-sum(ifelse((coverage$y_mod > coverage$yLW ) & (coverage$y_mod < coverage$yUP) , 1, 0))

    if (plot != FALSE) {
      dev.new() 
    plot(1:dim(coverage)[1], coverage$y_mod, type = "p",xlab="Domains",ylab="")
    polygon(c(1:dim(coverage)[1],rev(1:dim(coverage)[1])),c(coverage$yLW,rev(coverage$yUP)),col = "grey75", border = FALSE)
    points(1:dim(coverage)[1], coverage$y_mod)
    lines(1:dim(coverage)[1], coverage$yUP, col="red",lty=2)
    lines(1:dim(coverage)[1], coverage$yLW, col="red",lty=2)
    legend('topleft',c(sae.name[i],paste("Direct CI ",level,"%",sep="")),lty=c(NA,2),pch=c(1,NA),col=c("black","red"), cex=0.8,bty = "n")
    
    readline(prompt="Press [enter] to continue...")
    
    }
  }

  output <- data.frame(methods=sae.name,included=cov)
  all.output <- data.frame(output)
}