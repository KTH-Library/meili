% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/client.R
\name{wait_for_status}
\alias{wait_for_status}
\title{Wait for status of a Meili task}
\usage{
wait_for_status(task, verbose = FALSE)
}
\arguments{
\item{task}{the identifying number of the task}

\item{verbose}{set to TRUE for more talkative output}
}
\description{
Returns TRUE if no problem, returns FALSE and
writes an error message if an error occured
}
