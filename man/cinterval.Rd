\name{cinterval}
\alias{cinterval}
\title{Confident interval analysis}
\description{
\code{cinterval} analize the SAE estimates with respect to the confident inteval of the direct estimate.
}

\usage{
cinterval(data,dir,sae,v.dir,mse.sae,level=0.95,plot=F)
}

\arguments{
\item{data}{a data frame containing the direct and small area estimates among with their variance, e.g. \code{\link{SAEval_example}}.}
 \item{dir}{formula identifing the direct estimates.}
 \item{sae}{formula identifing the small area estimates.}
 \item{v.dir}{formula identifing the direct estimates variance.}
 \item{mse.sae}{formula identifing the small area estimates mean squared error.}
 \item{level}{double number. The confidence level represents the proportion of correspondingly confedent inteval that end up containing the true value of the parameter (default=0.95).}
  \item{plot}{logical scalar. Should the plot be produced (default=FALSE)?. See also 'Details'.}
}

\details{
This diagnostic measures for each SAE estimators the number of estimates that fall between the upper and lower bound of the direct estimates confidence intervals.

If \code{plot=TRUE} the SAE estimates are plotted with the direct estimates confindent interval to analyze the distributions.

The small area with both direct estimate and variance of the direct estimates equal to NA value are automatically removed from the data.
}

\value{
Object of class \code{data.frame}. The data frame contains information for the small area estimators (\code{methods}) about the number of SAE estimates included in the confident interval (\code{included}) and the number of overlapping confident intervals (\code{overlap}).
}
\author{
Developed by Andrea Fasulo
}
\references{
Brown, G., Chambers, R., Heady, P., Heasman, D. (2001), Evaluation of small area estimation methods - An application to unemployment estimates from the UK LFS, in Proceedings of Statistics Canada Symposium 2001: Achieving Data Quality in a Statistical Agency: A Methodological Perspective, Statistics Canada.

Mukhopadhyay, P. K., McDowell, A. (2011). Small area estimation for survey data analysis using SAS software, http://support.sas.com/rnd/app/papers/smallarea.pdf.

Srivastava, A. K., Sud, U. C., Chandra, H. (2007). Small area estimation - An application to National Sample Survey Data, Journal of the Indian Society of Agricultural Statistics, 61(2), 249-254.
}
\examples{
# Load example data
data(SAEval_example)

SAEval.cinterval<-cinterval(data=SAEval_example,
       dir=~y_d,
       sae=~y_syna+y_eblupa+y_spaznr+y_eblupb+y_synb+y_logis,
       v.dir=~mse_d,
       mse.sae=~mse_sa+mse_eba2+mse_spaznr+mse_ebb+mse_sb+mse_log)

SAEval.cinterval

}
