\name{bias}
\alias{bias}
\title{Bias diagnostic}
\description{
\code{bias} diagnostic allows to evaluate how the model-based estimates are closed to the unbiased direct estimates.
}
\usage{
bias(data,dir,sae,scatterplot=FALSE,main=NULL)
}
\arguments{
\item{data}{a data frame containing the direct estimates among with the small area estimates, e.g. \code{\link{SAEval_example}}.}
 \item{dir}{formula identifing the direct estimates.}
 \item{sae}{formula identifing the small area estimates.}
 \item{scatterplot}{logical scalar. Should the scatterplot of the estimates be produced (default=FALSE)?. See also 'Details'.}
 \item{main}{optionally, if a string is set in \code{main} it will be used as the scatterplot main title. The defualt main title is the name of the direct
 estimator versus the SAE names.}
 }
 
\details{
\code{bias} tests whether the model based estimates are closed to the direct estimates. A parametric test for the slope and for the intercept is carried out to check the unbiasedness of the model predictions. A square-root of the estimates is required when the homoskedasticity assumption underpinning the OLS fitting method is not satisfied.  The Goldfeld and Quandt homoscedasticity test is provided, to check such constant variances.

The use of this diagnostic is straightforward when the focus of interest is on small area totals since unbiased direct estimators of such totals are typically available.

If \code{scatterplot=TRUE} the SAE estimates (X-axis) are plotted on a cartesian plane against the direct estimates (Y-axis) to verify if there is a departure of the Y = X (red line) from the regression line between model based and direct estimates (black line).

The small area with direct estimate equal to NA value are automatically removed from the data.
}

\value{
Object of class \code{list}. The list contains up to 2 objects:
\item{output1}{a data frame containing for the small area estimates of interest (\code{methods}), the intercept (\code{b0}), the slope (\code{b1}) and the R-squared (\code{R2}) values among with the F-test (\code{F}) and Goldfeld and Quandt test (\code{GQ_Test}).}
\item{output2}{a data frame containing for the trasformed small area estimates of interest (\code{methods}) the intercept (\code{b0}), the slope (\code{b1})and the R-squared (\code{R2}) values among with F-test (\code{F}) and Goldfeld and Quandt test (\code{GQ_Test}).}

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

SAEval.bias<-bias(data=SAEval_example,
dir=~y_d,
sae = ~y_syna+y_eblupa+y_spaznr+y_eblupb+y_synb+y_logis)

SAEval.bias

}