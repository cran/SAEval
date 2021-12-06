
calibration <- function (data,dir,sae,area)
  
{
  
dir.name<-all.vars(dir)
sae.name<-all.vars(sae)
area.name<-all.vars(area)

calib<-list()

for (i in 1:length(area.name))
  
{

  
data.s<-data[,c(area.name[i],dir.name,sae.name)]

output5<-data.frame(matrix(0,nrow=length(unique(data.s[,area.name[i]])),ncol=length(sae.name)))
                
data.s<-data.s[!is.na(data.s[,dir.name]),]

estimates5<-aggregate(data.s,by=list(area1=data.s[,area.name[i]]),sum)[,-c(1,2)]
output5<-((estimates5[,sae.name]-estimates5[,dir.name])/estimates5[,dir.name])*100
output5<-data.frame(output5)
colnames(output5)<-sae.name

calib[[i]]<-output5

}

names(calib) <- area.name

all.output<-calib


}
