% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plots accidents on a US state map}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{The number of a state (numeric or integer) in the US used in the FARS data
sets.}

\item{year}{The year of analysis (numeric or integer)}
}
\value{
Returns a plot of the accidents of \code{state.num} and
\code{year}. Returns an error message if the no match for state or year is found in the data.
}
\description{
Accepts a state number and year and plots accidents on a map.
The state number should be integer or numerical. Returns error message and terminates
if no match in in the FARS data if no corresponding data file is found.
}
\examples{
\dontrun{
fars_map_state(45, 2015)

# Results in an error
fars_map_state(45, 2016)
fars_map_state(60, 2015)
}

}
