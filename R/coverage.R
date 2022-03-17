
coverage <- function (data,dir,sae,v.dir,mse.sae,alfa=.05)
  
{

dir.name<-all.vars(dir)
v.dir.name<-all.vars(v.dir)
sae.name<-all.vars(sae)
mse.sae.name<-all.vars(mse.sae)

data.s<-data[,c(dir.name,v.dir.name,sae.name,mse.sae.name)]
estimates2 <- data.s[!is.na(data.s[, v.dir.name])&data.s[, v.dir.name]!=0, ]


output<-list(0)
a<-"Accepted the H0: The overlap is 95%"
b<-"Rejected the H0: The overlap is 95%"


for (i in 1:length(sae.name))
  
{
  
  z<-1.96*(1+(sqrt(estimates2[,mse.sae.name[i]])/sqrt(estimates2[,v.dir.name])))^-1*
    sqrt(1+(estimates2[,mse.sae.name[i]]/estimates2[,v.dir.name]))
  xUP<-estimates2[,sae.name[i]]+z*sqrt(estimates2[,mse.sae.name[i]])
  xLW<-estimates2[,sae.name[i]]-z*sqrt(estimates2[,mse.sae.name[i]])
  yUP<-estimates2[,dir.name]+z*sqrt(estimates2[,v.dir.name])
  yLW<-estimates2[,dir.name]-z*sqrt(estimates2[,v.dir.name])
  coverage<-data.frame(y_d=estimates2[,dir.name],y_mod=estimates2[,sae.name[i]],xUP,xLW,yUP,yLW)
  flag<-ifelse((coverage$y_mod>coverage$y_d & coverage$xLW < coverage$yUP) | (coverage$y_mod<coverage$y_d & coverage$yLW < coverage$xUP) ,1,0)

  np_test<-binom.test(c(sum(flag), abs(sum(flag-1))), p = 0.95)
  
  output3<-data.frame(non_coverage=as.numeric(np_test$parameter)-as.numeric(np_test$statistic),domains=as.numeric(np_test$parameter),non_overlap=as.numeric(1-np_test$estimate),p_value=as.numeric(np_test$p.value))
  output3$results<-ifelse(output3$p_value<alfa,b,a)
  output3<-cbind(methods=sae.name[i],output3)
  output[[i]]<-output3

}


output<-do.call(rbind,output)

all.output<-data.frame(output)

}


