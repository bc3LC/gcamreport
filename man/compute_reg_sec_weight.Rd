% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{compute_reg_sec_weight}
\alias{compute_reg_sec_weight}
\title{compute_reg_sec_weight}
\usage{
compute_reg_sec_weight(dt)
}
\arguments{
\item{dt}{dataset with the following columns: scenario, region, var (reporting variable), year, value}
}
\value{
dataset with `reg_sec_weight` column.
}
\description{
An internal function designed to compute the regional weights of a set of variables. The World region
is considered as the annual unit.
}
