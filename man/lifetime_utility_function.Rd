% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_consistent_preferences.R
\name{lifetime_utility_function}
\alias{lifetime_utility_function}
\title{Model of time consistent preferences utility function}
\usage{
lifetime_utility_function(c_t, c_t_1, start, periods, beta, delta)
}
\arguments{
\item{c_t}{Value of the consumption today}

\item{c_t_1}{Value of the future consumption}

\item{start}{Equals to 0 if the consumption begin today, any other value means that the consumption
starts in the future}

\item{periods}{Number of periods in the future that we want to discount exponentially}

\item{beta}{Discount rate relative to today}

\item{delta}{Discount rate fo eah period in the future}
}
\value{
Present value of utility after discounting the future utility in today terms
}
\description{
Model of time consistent preferences utility function
}
