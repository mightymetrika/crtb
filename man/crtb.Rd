% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crtb.R
\name{crtb}
\alias{crtb}
\title{Complementary Resampling of Tags in Blocks}
\usage{
crtb(
  dat,
  pooled = TRUE,
  rowwise = TRUE,
  tie_thresh = 0.5,
  replace = TRUE,
  sample_fun = NULL,
  grp_var = NULL
)
}
\arguments{
\item{dat}{A data frame or vector containing the data to resample.}

\item{pooled}{Logical; if \code{TRUE} (default), data from all groups are
pooled together for resampling. If \code{FALSE}, resampling is performed
separately for each group.}

\item{rowwise}{Logical; applicable only when \code{pooled = TRUE}.
If \code{TRUE} (default), tagging is done row-wise across groups.
If \code{FALSE}, tagging is done column-wise within each group.}

\item{tie_thresh}{Numeric; a threshold between 0 and 1 to decide if there are
too many ties in the resampled data. If the proportion of unique resampled
tags is less than \code{tie_thresh}, the function will return \code{NULL}
and issue a warning. Default is \code{0.5}.}

\item{replace}{Logical; if \code{TRUE} (default), resampling is done with
replacement.}

\item{sample_fun}{A user-defined function for custom sampling. This function
should accept a vector of tags (the data to be resampled) and return a
resampled vector of the same length. If \code{NULL} (default), the standard
\code{sample} function is used with the \code{replace} argument.}

\item{grp_var}{Character; name of the column that contains group labels when
data is in long format (i.e., one observation per row with a column for
group membership). If \code{NULL} (default), assumes data is in wide format
with groups as separate columns. When \code{pooled = TRUE}, groups are combined
for resampling; when \code{pooled = FALSE}, resampling is performed separately
within each group.}
}
\value{
A list containing two elements:
\item{crdat}{The complementary resampled data.}
\item{ordat}{The original resampled data.}
}
\description{
Performs complementary resampling of tags in blocks (crtb) on the provided
dataset. This function implements crtb allowing for either pooled or non-pooled
resampling of data groups. This method is inspired by the concept of
complementary pairs subsampling, Shah & Samworth (2013), and attempts to
approximate the method in the realm of resampling.
}
\details{
The \code{crtb} function implements complementary resampling of tags in blocks
based on the concept of complementary pairs subsampling, Shah & Samworth (2013).

When \code{pooled = TRUE}, data from all groups are pooled together, and
resampling is performed using a combined tagging scheme.

When \code{pooled = FALSE}, the function applies the resampling procedure
separately to each group.

The function supports two data formats for grouped data:
\itemize{
\item Wide format (default): Each group is represented as a separate column
in the data frame
\item Long format: Data contains a column specifying group membership
(specified via \code{grp_var})
}
}
\examples{
# Example with a data frame and pooled = TRUE
data <- data.frame(group1 = rnorm(100), group2 = rnorm(100))
resampled_data <- crtb(data)

# Example with a data frame and pooled = FALSE
resampled_data_np <- crtb(data, pooled = FALSE)

# Example with a vector
vector_data <- rnorm(100)
resampled_vector <- crtb(vector_data)

# Example with rowwise = FALSE (only when pooled = TRUE)
resampled_data_colwise <- crtb(data, rowwise = FALSE)

# Custom sampling function
sample_fun <- function(x) {
  return(sample(x, replace = TRUE))
}

out_custom <- crtb(data, rowwise = TRUE, sample_fun = sample_fun)

# Example with pooled resampling using grp_var to handle unequal group size
dat <- data.frame(obs1 = rpois(7,5),
                  obs2 = rpois(7,9),
                  tx = c("T", "T", "T", "T", "C", "C", "C"))

uneq_grps <- crtb(dat, grp_var = "tx")

}
\references{
Shah, R. D., & Samworth, R. J. (2013). Variable Selection with Error Control:
Another Look at Stability Selection. \emph{Journal of the Royal Statistical
Society: Series B (Statistical Methodology)}, 75(1), 55–80.
\doi{10.1111/j.1467-9868.2011.01034.x}
}
