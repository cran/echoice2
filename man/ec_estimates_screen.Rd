% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/echoice2.R
\name{ec_estimates_screen}
\alias{ec_estimates_screen}
\title{Summarize attribute-based screening parameters}
\usage{
ec_estimates_screen(est, quantiles = c(0.05, 0.95))
}
\arguments{
\item{est}{is an 'echoice2' draw object (list) from a model with attribute-based screening}

\item{quantiles}{quantile for CI}
}
\value{
tibble with screening summaries
}
\description{
Summarize attribute-based screening parameters from an attribute-based screening model in 'echoice2'
}
\examples{
#run MCMC sampler (use way more than 20 draws for actual use)
data(icecream)
est_scr_icecream <- vd_est_vdm_screen(icecream\%>\%dplyr::filter(id<30), R=20, cores=2)
#summarise draws of screening probabilities
ec_estimates_screen(est_scr_icecream)
#Note: There is no variance in this illustrative example - more draws are needed

}
