% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_consistent_preferences.R
\name{exponential_discount}
\alias{exponential_discount}
\title{Exponential discounting of future utility}
\usage{
exponential_discount(consumption, beta, delta, periods)
}
\arguments{
\item{consumption}{value of the utility function at time t}

\item{beta}{Discount rate relative to today}

\item{delta}{Discount rate fo eah period in the future}

\item{periods}{Number of periods in the future that we want to discount exponentially}
}
\value{
Exponentially discounted value of utility function
}
\description{
Exponential discounting of future utility
}
