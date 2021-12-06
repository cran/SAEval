\name{coverage}
\alias{coverage}
\title{Coverage diagnostic}
\description{
\code{coverage} diagnostic tests the validity between the 95\% adjusted confidence intervals of the model based estimates making comparison with the corresponding adjusted confidence intervals for the direct estimates.
}

\usage{
coverage(data,dir,sae,v.dir,mse.sae,alfa=0.05)
}

\arguments{
\item{data}{a data frame containing the direct and small area estimates among with their variance, e.g. \code{\link{SAEval_example}}.}
 \item{dir}{formula identifing the direct estimates.}
 \item{sae}{formula identifing the small area estimates.}
 \item{v.dir}{formula identifing the direct estimates variance.}
 \item{mse.sae}{formula identifing the small area estimates mean squared error.}
 \item{alfa}{double number. The significance level of the non-parametric Binomial test (default=0.05).}
}

\details{
This diagnostic measures the overlap between the confidence intervals, which is expected to be not significantly
different from the 95\% of the numbers of small areas.

The small area with both direct estimate and variance of the direct estimates equal to NA value are automatically removed from the data.
}

\value{
Object of class \code{data.frame}. The data frame contains information for the small area estimators (\code{methods}), non-coverage total (\code{non_coverage}), number of small area domains (\code{domains}), non-overlap ratio (\code{non_overlap}), p-value for Binomial statistic (\code{p_value}) and the test result (\code{results}).
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

SAEval.coverage<-coverage(data=SAEval_example,
       dir=~y_d,
       sae=~y_syna+y_eblupa+y_spaznr+y_eblupb+y_synb+y_logis,
       v.dir=~mse_d,
       mse.sae=~mse_sa+mse_eba2+mse_spaznr+mse_ebb+mse_sb+mse_log)

SAEval.coverage

}
