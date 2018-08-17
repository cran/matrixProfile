\name{std}
\alias{std}
\title{Sample standard deviation}
\description{
Calculates sample standard deviation of input data.
}
\usage{
std(x)
}
\arguments{
 \item{x}{A given input data.}
}
\details{
This function is slightly different from the \pkg{base} function \code{sd}.
}
\value{
An sample standard deviation of given data.
}
\references{
\url{https://en.wikipedia.org/wiki/Standard_deviation}
}
\author{
Donghwan Kim\cr
\email{ainsuotain@hanmail.net}
\email{donhkim9714@korea.ac.kr}
\email{dhkim2@bistel.com}
}
\examples{
x <- 1:10
sd(x) # for comparison
std(x) # see difference
}

