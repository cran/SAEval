
gof <- function (data, dir, sae,v.dir,mse.sae,alfa=0.05)
  
{

dir.name<-all.vars(dir)
v.dir.name<-all.vars(v.dir)
sae.name<-all.vars(sae)
mse.sae.name<-all.vars(mse.sae)

data.s<-data[,c(dir.name,v.dir.name,sae.name,mse.sae.name)]

#data.s[,dir.name]<-ifelse(is.na(data.s[,dir.name]),0,data.s[,dir.name])
#estimates3<-data.s[data.s[,dir.name]>0,]

estimates3<-data.s[!is.na(data.s[,dir.name]),]
estimates3<-data.s[!is.na(data.s[,v.dir.name]),]

w<-list(0)
W<-numeric(0)
ff<-numeric(0)
a<-"Accepted the H0: E(Direct estimates)= Model based Estimates"
b<-"Rejected the H0: E(Direct estimates)= Model based Estimates"

for (i in 1:length(sae.name))
  
{
  
  w[[sae.name[i]]]<-data.frame((estimates3[,sae.name[i]]-estimates3[,dir.name])^2/(estimates3[,v.dir.name]+estimates3[,mse.sae.name[i]]))
  W[i]<-sum(w[[sae.name[i]]])
  W<-W[!is.na(W)]
  c2<-qchisq(1-alfa, length(estimates3[,dir.name])-1)
  p_value<-1-pchisq(W,length(estimates3[,dir.name])-1)
  ff[i]<-sae.name[i]
  ff<-ff[!is.na(ff)]
  output4<-data.frame(methods=ff,W,c2,p_value)
  
}

output4$results<-ifelse(output4$c2<output4$W ,b,a)

all.output<-data.frame(output4)

}

