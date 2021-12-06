\name{calibration}
\alias{calibration}
\title{Calibration diagnostic}
\description{
\code{calibration} diagnostic refers to the calibration property of model estimates, according to which they should not differ from the direct estimates when aggregated at appropriate large domain levels. Computing this diagnostic we obtain an accurate measure of the calibration property of the model estimates, providing also an evidence of the presence/absence of spatial bias/autocorrelation.
}
\usage{
calibration(data,dir,sae,area)}
\arguments{
\item{data}{a data frame containing the direct and small area estimates among with their variance, e.g. \code{\link{SAEval_example}}.}
 \item{dir}{formula identifing the direct estimates.}
 \item{sae}{formula identifing the small area estimates.}
 \item{area}{formula identifing the area for which the \code{calibration} diagnostic is computed.}
}
\details{
\code{calibration} compute the relative difference
between the aggregated model-based estimates and the aggregated direct estimates.

The small area with both direct estimate and variance of the direct estimates equal to NA value are automatically removed from the data.
}
\value{
Object of class \code{list}. The list contains objects equal to the number of larger domain specified in \code{area}. Each object will contains the \code{calibration} diagnostic for all the modes of the area.
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

SAEval.calibration<-calibration(data=SAEval_example,
       dir=~y_d,
       sae=~y_syna+y_eblupa+y_spaznr+y_eblupb+y_synb+y_logis,area=~nuts0+nuts1+nuts2)

SAEval.calibration
}