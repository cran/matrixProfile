\name{stamp}
\alias{stamp}
\title{Scalable Time series Anytime Matrix Profile (stamp)}
\description{
Calculates a matrix profile of given data using STAMP algorithm.
}
\usage{
stamp(q, t, by = 10, isPlot = FALSE)
}
\arguments{
 \item{q}{A query data for dot product.}
 \item{t}{A timeseries data for analysis.}
 \item{by}{A parameter that indicates the progress of the process in the process of calculating the matrix profile. For example, if \code{by} is 10, the text is displayed in the console window every 10 percent.}
 \item{isPlot}{A parameter that determines whether or not to draw a plot in the middle of calculating a matrix profile. The default value is \code{FALSE}.}

}
\details{
The matrix profile is calculated by the self join method using the STAMP algorithm. One of the key features of the STAMP algorithm is the attribute anytime. In other words, because the matrix profile is computed rather than randomly, the computation speed is the same, but it is quickly optimized.}

\value{
An object of class \code{stamp.models}.

 \item{MP}{A matrix profile computed by given data.}
 \item{MPI}{A matrix profile index computed by given data.}
 \item{MTI}{A motif index of matrix profile. Unlike in the original paper, it denotes the pair of motif index with the smallest value of matrix profile.}
}

\references{
Yeh, C. C. M., Zhu, Y., Ulanova, L., Begum, N., Ding, Y., Dau, H. A., ... & Keogh, E. (2016) <DOI:10.1109/ICDM.2016.0179>. \emph{2016 IEEE 16th International Conference on Data Mining (ICDM)}, Barcelona, 2016, pp. 1317-1322.
\cr
\url{http://www.cs.ucr.edu/~eamonn/MatrixProfile.html}
}
\author{
Donghwan Kim\cr
\email{ainsuotain@hanmail.net}
\email{donhkim9714@korea.ac.kr}
\email{dhkim2@bistel.com}
}
\note{
This package is an early version and will be updated in the neae future. Also note that it is very slow for data with more than 10,000 data points. Since it is not optimized basic functions(e.g. \code{movmean}, \code{movstd}) for computation and is due to \code{R}'s own limitations.
}
\seealso{
 \code{\link{mass}} (in package \pkg{matrixProfile})
}
\examples{
# data input
dt = AirPassengers
dt = as.vector(dt)

# generates matrix profile
stamp <- stamp(q = dt[1:12], t = dt[-c(1:12)])

# plotting
par(mfrow = c(2,1))
plot(dt, type = "l", main = "Original Timeseries")
plot(stamp$MP, type = "l", main = "Matrix Profile", xlim = c(0, length(dt)))
}
\keyword{matrix profile}
\keyword{distance profile}
\keyword{time series motif discovery}
