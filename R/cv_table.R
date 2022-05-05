
cv_table <- function (data,cv,boxplot=FALSE)
  
{
  
  cv.name<-all.vars(cv)
  data.s<-data[,cv.name]
  a<-list()
  
  for (i in cv.name)
  {
    b<-ifelse(data.s[,i]<=.165,1,
              ifelse(data.s[,i]<=.333,2,3))
    a[[i]]<-data.frame(table(b))
    
  }
  
  suppressWarnings(Merged<-Reduce(function(x, y) merge(x, y,all=T,by="b"),a))
  Merged[is.na(Merged)]<-0
  Merged$b<-as.numeric(as.character(Merged$b))
  Merged<-Merged[order(Merged$b),]
  colnames(Merged)<-c("cv_threshold",cv.name)
  Merged[,1]<-c("0 - 0.165", "0.166 - 0.333", ">0.333")
  if (sum(Merged[,2])<sum(Merged[,3]))
  {Merged<-rbind(Merged,c("NA",sum(Merged[,3])-sum(Merged[,2]),rep(0,length(cv.name)-1)))
  }
  
  if (boxplot!=FALSE)
  {
    dev.new()
    boxplot(data.s)
  }
  all.output<-data.frame(Merged)
  
}