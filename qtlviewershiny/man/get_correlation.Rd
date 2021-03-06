% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qtlapi.R
\name{get_correlation}
\alias{get_correlation}
\title{Get the correlation.}
\usage{
get_correlation(
  dataset,
  id,
  dataset_correlate = NULL,
  intcovar = NULL,
  use_qr = TRUE
)
}
\arguments{
\item{dataset}{The dataset identifier.}

\item{id}{The identifier.}

\item{dataset_correlate}{The dataset to correlate against.}

\item{intcovar}{The interactive covariate, sample.column in covar.info.}

\item{use_qr}{TRUE to use QR decomposition (FASTER).}
}
\value{
A tibble with the correlation and annotations.
}
\description{
Get the correlation.
}
