\name{map_sae}
\alias{map_sae}
\title{Map the disagregated estimates and the coefficients of variation.}
\description{
\code{map_sae} produces geographical maps for the small area estimates or the direct estimaes among with their CVs.
}
\usage{
map_sae(shapefile,
data,
area,
indicators,
color=c("green","red"),
breaks=FALSE,
main=FALSE,
output_data=FALSE)
}
\arguments{
\item{shapefile}{object of class \code{sf} and \code{data.frame} as defined by the \code{sf} package containing shapefile informations, e.g. \code{\link{sa_shp}}. See also 'Details'.}
 \item{data}{data frame containing for the area of interest the information to be visualized,  e.g. \code{\link{SAEval_example}}.}
 \item{area}{formula identifing the area of interest.}
  \item{indicators}{formula identifing the variables to be visualized.}
  \item{color}{a \code{vector} of 2 color defining the lowest and highest values in the plot}
  \item{breaks}{list containing the endpoints for each indicator of interest (default=FALSE).}
  \item{main}{logical scalar. Should the maps include a main title (default=FALSE)?. See also 'Details'.}
  \item{output_data}{logical scalar. Should the funtion returns a data frame including the map data among with the indicators of interest (default=FALSE)?. See also 'Details'.}
 }
 
\details{
\code{shapefile} object can be created with the \code{sf} package using the function \code{st_read}.
If \code{main} is equal to \code{TRUE} the name of the indicator will be used as main title of the map.
When \code{output_data} is equal to \code{TRUE} a map data object is returned so can be easaly maneged using \code{ggplot} for a better graphical personalizzation. 
}

\value{

Returns maps, and, if selected, a data.frame containing the mapdata enriched with the indicators of interest.

}
\author{
Developed by Andrea Fasulo
}
\references{
Pebesma E., et al.,2021, "sf: Simple Features for R", CRAN repository https://CRAN.R-project.org/package=sf
}

\examples{

# Load example data and shape file
data(SAEval_example);data(sa_shp)

SAEval_example$cv_d<-sqrt(SAEval_example$mse_d)/SAEval_example$y_d

SAEval_example$cv_sa<-sqrt(SAEval_example$mse_sa)/SAEval_example$y_syna
\donttest{
# Without using breaks
map_sae(shapefile=sa_shp,data=SAEval_example,area=~sa,indicators=~y_d+cv_d+y_syna+cv_sa,main=TRUE)

# Using breaks
map_sae(shapefile=sa_shp,data=SAEval_example,area=~sa,indicators=~y_d+cv_d+y_syna+cv_sa,
        breaks=list(seq(0,40000,5000),seq(0,1.5,0.15),seq(0,40000,5000),seq(0,1.5,0.15)),main=TRUE)
}

}