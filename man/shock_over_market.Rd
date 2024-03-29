% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microeconomics.R
\name{shock_over_market}
\alias{shock_over_market}
\title{Analize a market after a shock:}
\usage{
shock_over_market(
  market,
  curve,
  shock_name = "Tax",
  p_delta = 0.15,
  q_delta = 0,
  slope_change = 0,
  percent = TRUE
)
}
\arguments{
\item{market}{A simple market with supply and demand produced by \code{create_market}
function. The element is a list of class \strong{market_curves}}

\item{curve}{String with the curve(s) name that get the shock.
Use "demand" or "supply"}

\item{shock_name}{A name from the shock. Default value: "Tax"}

\item{p_delta}{\code{p_delta} and \code{q_delta} works together with the \code{percent} argument.
If \code{percent} is \code{TRUE} then all prices are multiplied by the value declared. Otherwise
if \code{percent} is \code{FALSE} the curve moves in or out by a constant value.}

\item{q_delta}{p_delta \code{p_delta} and \code{q_delta} works together with the \code{percent} argument.
If \code{percent} is \code{TRUE} then all prices are multiplied by the value declared. Otherwise
if \code{percent} is \code{FALSE} the curve moves in or out by a constant value.}

\item{slope_change}{To wich value the slope of the curve should change.}

\item{percent}{If the shock is a percentage (15\% tax over supply) use \code{TRUE},
else, a constant is summed to the curve.}
}
\value{
List with:
\itemize{
\item Original market plot
\item Income and substitution effect plot
\item Consumer and producer surplus before shock
\item Consumer and producer surplus after shock
\item Table with
\itemize{
\item Elasticities at equilibrium (before and after)
\item Surplus (before and after)
\item Income and substitution effect
}
}
}
\description{
Analize a market after a shock:
}
