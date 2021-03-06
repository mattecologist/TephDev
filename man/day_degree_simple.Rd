% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weather_functions.R
\name{day_degree_simple}
\alias{day_degree_simple}
\title{Simple day degree model}
\usage{
day_degree_simple(
  tlow = 4.5,
  thigh = 100,
  tempvec = tempvec,
  start.date = "2015-08-31"
)
}
\arguments{
\item{tlow}{Low temperature threshold}

\item{thigh}{High temperature threhold}

\item{tempvec}{Vector of temperatures (and dates)}

\item{start.date}{Date to start analysis on}
}
\description{
Uses a single-triangle method
}
