\name{SAEval_example}
\Rdversion{1.0}
\alias{SAEval_example}
\docType{data}
\title{
Example dataset for the evaluation of Small Area Estimates
}
\description{
\code{SAEval_example} contains a \code{data.frame} with direct and indirect estimates for unplunned domain among with their variance.
}
\usage{
data(SAEval_example)
}
\format{
  \code{SAEval_example} is a data frame with 107 domains and 18 variables:
  \describe{
    \item{\code{sa}}{domain of interest codes}
    \item{\code{nuts1}}{NUTS1 codes} 
    \item{\code{nuts2}}{NUTS2 codes} 
    \item{\code{nuts0}}{NUTS0 codes} 
    \item{\code{y_d}}{direct estimated} 
    \item{\code{mse_d}}{variance of direct estimates}    
    \item{\code{y_syna}}{unit level synthetic estimates} 
    \item{\code{mse_sa}}{MSE of unit level synthetic estimates}     \item{\code{y_eblupa}}{unit level EBLUP estimates} 
    \item{\code{mse_eba2}}{MSE of unit level EBLUP estimates}
    \item{\code{y_spaznr}}{unit level EBLUP estimates with spatial correlation of random effects} 
    \item{\code{mse_spaznr}}{MSE of unit level EBLUP estimates with spatial correlation of random effects} 
    \item{\code{y_eblupb}}{area level EBLUP estimates} 
    \item{\code{mse_ebb}}{MSE of area level EBLUP estimates}
    \item{\code{y_synb}}{area level synthetic estimates} 
    \item{\code{mse_sb}}{MSE of area level synthetic estimates}     \item{\code{y_logis}}{unit level EBLUP type logit estimates} 
    \item{\code{mse_log}}{MSE of unit level EBLUP type logit estimates} 
    }
}
\examples{
# Load example data
data(SAEval_example)
summary(SAEval_example)
# being the domain unplunned there are 7 areas without direct estimates
dim(SAEval_example[SAEval_example$y_d==0,])
}
\keyword{datasets}
