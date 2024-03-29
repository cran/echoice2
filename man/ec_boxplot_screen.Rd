% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/echoice2.R
\name{ec_boxplot_screen}
\alias{ec_boxplot_screen}
\title{Generate Screening probability boxplot}
\usage{
ec_boxplot_screen(draws, burnin = 100)
}
\arguments{
\item{draws}{A list, 'echoice2' draws object, from a model with attribute-based screening}

\item{burnin}{burn-in to remove}
}
\value{
A ggplot2 plot containing traceplots of draws
}
\description{
Generate Screening probability boxplot
}
\examples{
\donttest{
data(icecream)
#run MCMC sampler (use way more than 20 draws for actual use
icecream_scr_est <- icecream \%>\% dplyr::filter(id<20) \%>\% vd_est_vdm_screen(R=20, cores=2)
ec_boxplot_screen(icecream_scr_est, burnin = 1)
}

}
\seealso{
\code{\link[=ec_draws_MU]{ec_draws_MU()}} to obtain MU_theta draws,
\code{\link[=ec_trace_screen]{ec_trace_screen()}} to generate traceplot
}
