
cv_table <- function (data,cv)
  
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
  
  Merged<-Reduce(function(x, y) merge(x, y,all=T,by="b"),a)
  Merged[is.na(Merged)]<-0
  colnames(Merged)<-c("cv_threshold",cv.name)
  Merged[,1]<-c("0 - 0.165", "0.166 - 0.333", ">0.333")
  
  all.output<-data.frame(Merged)
  
}