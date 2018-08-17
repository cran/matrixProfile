\name{mass}
\alias{mass}
\title{Mueen's ultra-fast Algorithm for Similarity Search (MASS)}
\description{
Calculates a sliding dot prodocts of given data.
}
\usage{
mass(q, t)
}
\arguments{
 \item{q}{A query data for dot product.}
 \item{t}{A timeseries data for analysis.}
}
\value{
Dot products between query and all subsequences in Timeseries. In the paper, we can implicitly construct a distance matrix with this output values that are the result of these dot products.
}
\references{
Yeh, C. C. M., Zhu, Y., Ulanova, L., Begum, N., Ding, Y., Dau, H. A., ... & Keogh, E. (2016) <DOI:10.1109/ICDM.2016.0179>. \emph{2016 IEEE 16th International Conference on Data Mining (ICDM)}, Barcelona, 2016, pp. 1317-1322.
\cr
\url{https://www.cs.unm.edu/~mueen/MASS_V2.m}

}
\author{
Donghwan Kim\cr
\email{ainsuotain@hanmail.net}
\email{donhkim9714@korea.ac.kr}
\email{dhkim2@bistel.com}
}

\examples{
dt = AirPassengers
dt = as.vector(dt)
par(mfrow = c(2,1))
plot(dt, type = "l")
dm <- mass(q = dt[1:10], t = dt[-c(1:10)])
plot(dm, tyep = "l")
}

