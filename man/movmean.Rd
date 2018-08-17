\name{movmean}
\alias{movmean}
\title{Moving mean}
\description{
Calculates moving mean of input data.
}
\usage{
movmean(x = x, w = w)
}
\arguments{
 \item{x}{A given input data.}
 \item{w}{A sliding window of length w.}
}
\value{
An array of local w-point mean values, where each mean is calculated over a sliding window of length w across neighboring elements of x. The window size is automatically truncated at the endpoints when there are not enough elements to fill the window. When the window is truncated, the average is taken over only the elements that fill the window. Output is the same size as x.
}
\references{
\url{https://kr.mathworks.com/help/matlab/ref/movmean.html?lang=en}
}
\author{
Donghwan Kim\cr
\email{ainsuotain@hanmail.net}
\email{donhkim9714@korea.ac.kr}
\email{dhkim2@bistel.com}
}
\examples{
x <- 1:10
movmean(x, 3)
}

