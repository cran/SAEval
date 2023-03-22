
cv_table <- function (data,cv,boxplot=FALSE)
  
{
  
  cv.name <- all.vars(cv)
  data.s <- data[, cv.name]
  a <- list()
  for (i in cv.name) {
    b <- ifelse(data.s[, i] <= 0.165, "0 - 0.165", ifelse(data.s[, 
                                                                 i] <= 0.333, "0.166 - 0.333", ">0.333"))
    a[[i]] <- data.frame(table(b))
  }
  suppressWarnings(Merged <- Reduce(function(x, y) merge(x, 
                                                         y, all = T, by = "b"), a))
  Merged[is.na(Merged)] <- 0
  
  colnames(Merged) <- c("cv_threshold", cv.name)
  
  Merged$b <- ifelse(Merged$cv_threshold=="0 - 0.165",1,
                     ifelse(Merged$cv_threshold=="0.166 - 0.333",2,3))
  Merged <- Merged[order(Merged$b), ]
  Merged$b <- NULL
  
  na<-sapply(data.s, function(x) sum(is.na(x)))
  
  if (sum(na)>0) {
    na<-data.frame(cbind(cv_threshold="NA", t(na)))
    Merged <- rbind(Merged,na )
  }
  if (boxplot != FALSE) {
    dev.new()
    boxplot(data.s)
  }
  all.output <- data.frame(Merged)
  
  
}