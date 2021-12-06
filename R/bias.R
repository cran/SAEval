
bias <- function (data,dir,sae,scatterplot=FALSE)
  
{
  dir.name<-all.vars(dir)
  sae.name<-all.vars(sae)
  data.s<-data[,c(dir.name,sae.name)]
  #data.s[,dir.name]<-ifelse(is.na(data.s[,dir.name]),0,data.s[,dir.name])
  data.s<-data.s[!is.na(data.s[,dir.name]),]
    
  B0<-numeric(0)
  B1<-numeric(0)
  R2<-numeric(0)
  b0<-numeric(0)
  b1<-numeric(0)
  r2<-numeric(0)
  
  F<-numeric(0)
  f<-numeric(0)
  GQ_Test<-numeric(0)
  gq_Test<-numeric(0)
  a<-"Reject the H0: b0=0 and b1=1"
  b<-"Accept the H0: b0=0 and b1=1"
  c<-"Reject the H0: residual area homoskedastic. "
  d<-"Accept the H0: residual area homoskedastic"
  sae.names.t<-c()
  
  for(i in sae.name)
    
  { 
    
    y<-(lm(data.s[,dir.name]~data.s[,i]))
    
    smr<-summary(y)
    if (scatterplot!=FALSE)
    {
      dev.new()
    plot(data.s[,dir.name]~data.s[,i],xlab=i,ylab="direct",main=paste("direct vs", i, sep=" "))
    abline(lm(data.s[,dir.name]~data.s[,i]))
    abline(c(0,1),col="red")
    legend("topleft", legend=c("Bisector", "Regression line"),
           col=c("red", "black"), lty=c(1,1),cex=0.8)
    }
    test<-gqtest(data.s[,dir.name]~data.s[,i])
    
    b0[i]<-as.vector(smr[[4]][[1]])
    b1[i]<-as.vector(smr[[4]][[2]])
    R2[i]<-as.vector(smr[[8]][[1]])
    
    F[i]<-ifelse(linearHypothesis(y,diag(2),c(0,1))$'Pr(>F'[2]<.05,a,b)
    GQ_Test[i]<-ifelse(test$p.value<0.05,c,d)
    
    
    if (test$p.value<0.05)
    {
      z<-(lm(sqrt(data.s[,dir.name])~sqrt(data.s[,i])))
      
      smrz<-summary(z)
      if (scatterplot!=FALSE)
      {
        dev.new()
      plot(sqrt(data.s[,dir.name])~sqrt(data.s[,i]),xlab=i,ylab="sqrt-direct",main=paste("sqrt-direct vs sqrt-",i, sep=""))
      abline(lm(sqrt(data.s[,dir.name])~sqrt(data.s[,i])))
      abline(c(0,1),col="red")
      legend("topleft", legend=c("Bisector", "Regression line"),
             col=c("red", "black"), lty=c(1,1),cex=0.8)
      }
      testz<-gqtest(data.s[,dir.name]~sqrt(data.s[,i]))
      
      B0[i]<-as.vector(smrz[[4]][[1]])
      B1[i]<-as.vector(smrz[[4]][[2]])
      r2[i]<-as.vector(smrz[[8]][[1]])
      
      f[i]<-ifelse(linearHypothesis(z,c("(Intercept)=0","sqrt(data.s[, i])=1"))$'Pr(>F'[2]<.05,a,b)
      
      gq_Test[i]<-ifelse(testz$p.value<0.05,c,d)
      
      sae.name.t<-paste("sqrt_",i,sep="")
    }
    if (scatterplot!=FALSE)
    {
    readline(prompt="Press [enter] to continue...")
    }
  }
  
  output1<-data.frame(methods=sae.name,b0,b1,R2,F,GQ_Test)
  output2<-data.frame(methods=sae.name.t,B0,B1,r2,f,gq_Test)
  colnames(output2) <-c("methods","b0","b1","R2","F","GQ_Test")
  rownames(output1) <- NULL
  rownames(output2) <- NULL
  
  all.output<-list(output1=output1,output2=output2)
  
}

