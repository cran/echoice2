% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/echoice2.R
\name{ec_boxplot_MU}
\alias{ec_boxplot_MU}
\title{Generate MU_theta boxplot}
\usage{
ec_boxplot_MU(draws, burnin = 100)
}
\arguments{
\item{draws}{A list, 'echoice2' draws object}

\item{burnin}{burn-in to remove}
}
\value{
A ggplot2 plot containing traceplots of draws
}
\description{
Generate MU_theta boxplot
}
\examples{
\dontrun{
data(icecream)
#run MCMC sampler (use way more than 50 draws for actual use
icecream_est <- icecream \%>\% dplyr::filter(id<100) \%>\% vd_est_vdm(R=20, cores=2)
ec_boxplot_MU(icecream_est, burnin=1)
}

}
\seealso{
\code{\link[=ec_trace_MU]{ec_trace_MU()}} to obtain traceplot
}
