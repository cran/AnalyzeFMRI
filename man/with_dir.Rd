\name{with_dir}
\alias{with_dir}
\title{Working directory}
\usage{
with_dir(new, code)
}
\arguments{
\item{new}{\verb{[character(1)]}\cr New working directory}

\item{code}{\code{[any]}\cr Code to execute in the temporary environment}
}
\value{
\code{[any]}\cr The results of the evaluation of the \code{code}
argument.
}
\description{
Temporarily change the current working directory.
}
\examples{
getwd()

with_dir(tempdir(), getwd())
}
\seealso{
\code{\link[=setwd]{setwd()}}
}
