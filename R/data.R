#' crtb_lm_sim time elapsed data
#'
#' This data set explores how long it takes to run the crtb_lm_sim() function on
#' different AWS EC2 instances under different numbers of simulation iterations
#' and bootstrap samples.
#'
#' The quoted run time definitions were taken from:
#' <https://www.r-bloggers.com/2017/05/5-ways-to-measure-running-time-of-r-code/>
#'
#' @format ## `crtb_lm_sim_runtimes`
#' A data frame with 39 rows and 7 columns:
#' \describe{
#'   \item{instance}{AWS EC2 instance type}
#'   \item{iterations}{Number of simulation iterations}
#'   \item{B}{Number of bootstrap samples}
#'   \item{user}{"CPU time spent by the current process"}
#'   \item{system}{"CPU time spent by the kernel (the operating system) on behalf
#'   of the current process"}
#'   \item{elapsed}{"wall clock time taken to execute the function...plus some
#'   benchmarking code wrapping it"}
#'   \item{Notes}{Notes}
#' }
"crtb_lm_sim_runtimes"
