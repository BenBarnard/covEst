% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cov.R
\name{cov}
\alias{cov}
\title{Covariance Matrix Calculation for (Potentially Grouped) Data}
\usage{
cov(x, ..., covEst = stats::cov)
}
\arguments{
\item{x}{data you would like the covariance matrix of. The parameter \code{x}
can be a \code{data.frame}, \code{grouped_df}, \code{resample}, or
\code{matrix}.}

\item{...}{other options passed to covarince estimation method}

\item{covEst}{covariance or precision matrix estimation method, as a function.
Defaults to \code{stats::cov}.}
}
\value{
list of matries with class \code{covariance} by group. These matrices
   may be covariance or precision matrices, depending on the function
   supplied to the \code{covEst} argument.
}
\description{
Covariance Matrix Calculation for (Potentially Grouped) Data
}
\details{
This function has the capability to calculate total and group-level
   covariance and precision matrices estimated from the given data. Specify
   group membership with the \code{group = "COLNAME"} syntax. Additionally,
   this function returns objects with an additional class: when this function
   returns group-level lists of matrices, each covariance or precision matrix
   will have the \code{R} classes \code{matrix} \emph{and} \code{covariance};
   likewise, total covariance or precision matrices are also returned with
   the classes \code{matrix} and \code{covariance}. Moreover, these matrices
   have an additional named attribute: \code{df}, for \emph{degrees of freedom}.
   The computational functionality of the \code{covariance} class will be
   explored in future updates.
}
\examples{
cov(iris[,1:4])
cov(iris, group = "Species")
cov(iris[,1:4], covEst = Haff_shrinkage)
}
