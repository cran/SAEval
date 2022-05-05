\name{cv_table}
\alias{cv_table}
\title{Coefficent of variation's table}
\description{
\code{cv_table} is used to analyse the coefficent of variation distribution of the chosen indicators.
}
\usage{
cv_table(data,cv,boxplot=FALSE)
}
\arguments{
\item{data}{a data frame containg the coefficient of variation for the direct and small area estimators}
\item{cv}{formula identifing the coefficient of variation.}
\item{boxplot}{logical scalar. Should the boxplot of the coefficient of variation be produced (default=FALSE)?.}
 }
 
\details{
\code{cv_table} allows to evaluate the cv of the different estimators with respect to some well-known thresholds given by Statistics Canada (2009). For cv below 0.165 there are no rescrictions to the dissemination, for cv in the range 0.166-0.333 is suggested a publication with a warning, for cv above 0.333 the dissemination is  not recommendent.
}

\value{
Object of class \code{data.frame}. The data frame contains informations about the number of cvs that fall within each class.
}
\author{
Developed by Andrea Fasulo
}
\references{
Statistics Canada, 2009, "Quality Guideline", Fifth edition, October 2009
}

\examples{
# Load example data
data(SAEval_example)

# cv for the direct estimates
SAEval_example$cvd<-sqrt(SAEval_example$mse_d)/SAEval_example$y_d
#cv for the synthetic estimates
SAEval_example$cvsae<-sqrt(SAEval_example$mse_sa)/SAEval_example$y_syna

cv_data<-SAEval_example[,c("cvd","cvsae")]

SAEval_cvtable<-cv_table(data=cv_data,
cv=~cvd+cvsae)

SAEval_cvtable

}