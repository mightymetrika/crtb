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

#' Simulation Results: type1_2var_uncor_n15_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_uncor_n15_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_uncor_n15_sim"

#' Simulation Results: type1_2var_uncor_n30_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_uncor_n30_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_uncor_n30_sim"

#' Simulation Results: type1_2var_uncor_n15_wide_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_uncor_n15_wide_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_uncor_n15_wide_sim"

#' Simulation Results: type1_2var_uncor_n30_wide_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_uncor_n30_wide_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_uncor_n30_wide_sim"

#' Simulation Results: type1_2var_cor_n15_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_cor_n15_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_cor_n15_sim"

#' Simulation Results: type1_2var_cor_n30_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_cor_n30_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_cor_n30_sim"

#' Simulation Results: type1_2var_cor_n15_wide_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_cor_n15_wide_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_cor_n15_wide_sim"

#' Simulation Results: type1_2var_cor_n30_wide_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_cor_n30_wide_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_cor_n30_wide_sim"

#' Simulation Results: type1_2var_uncor_n15_skew_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_uncor_n15_skew_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_uncor_n15_skew_sim"

#' Simulation Results: type1_2var_uncor_n30_skew_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_uncor_n30_skew_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_uncor_n30_skew_sim"

#' Simulation Results: type1_2var_uncor_n15_wide_skew_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_uncor_n15_wide_skew_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_uncor_n15_wide_skew_sim"

#' Simulation Results: type1_2var_uncor_n30_wide_skew_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_uncor_n30_wide_skew_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_uncor_n30_wide_skew_sim"

#' Simulation Results: type1_2var_cor_n15_skew_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_cor_n15_skew_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_cor_n15_skew_sim"

#' Simulation Results: type1_2var_cor_n30_skew_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_cor_n30_skew_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_cor_n30_skew_sim"

#' Simulation Results: type1_2var_cor_n15_wide_skew_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_cor_n15_wide_skew_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_cor_n15_wide_skew_sim"

#' Simulation Results: type1_2var_cor_n30_wide_skew_sim
#'
#' Results from simulation study
#'
#' @format ## `type1_2var_cor_n30_wide_skew_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"type1_2var_cor_n30_wide_skew_sim"

#' Simulation Results: pow_2var_uncor_n15_sim
#'
#' Results from simulation study
#'
#' @format ## `pow_2var_uncor_n15_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"pow_2var_uncor_n15_sim"

#' Simulation Results: pow_2var_uncor_n30_sim
#'
#' Results from simulation study
#'
#' @format ## `pow_2var_uncor_n30_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"pow_2var_uncor_n30_sim"

#' Simulation Results: pow_2var_uncor_n15_wide_sim
#'
#' Results from simulation study
#'
#' @format ## `pow_2var_uncor_n15_wide_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"pow_2var_uncor_n15_wide_sim"

#' Simulation Results: pow_2var_uncor_n30_wide_sim
#'
#' Results from simulation study
#'
#' @format ## `pow_2var_uncor_n30_wide_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"pow_2var_uncor_n30_wide_sim"

#' Simulation Results: pow_2var_cor_n15_sim
#'
#' Results from simulation study
#'
#' @format ## `pow_2var_cor_n15_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"pow_2var_cor_n15_sim"

#' Simulation Results: pow_2var_cor_n30_sim
#'
#' Results from simulation study
#'
#' @format ## `pow_2var_cor_n30_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"pow_2var_cor_n30_sim"

#' Simulation Results: pow_2var_cor_n15_wide_sim
#'
#' Results from simulation study
#'
#' @format ## `pow_2var_cor_n15_wide_sim`
#' A data frame with 2 rows and 7 columns:
#' \describe{
#'   \item{results}{Bootstrap method}
#'   \item{avg_bias}{Average bias across simulation iterations}
#'   \item{avg_se}{Standard error across simulation iterations}
#'   \item{coverage}{Proportion of confidence intervals that include true parameter}
#'   \item{ci_width}{Average confidence interval width}
#'   \item{converged}{Proportion of converged crtb}
#'   \item{rejection}{Rejection rate}
#' }
"pow_2var_cor_n15_wide_sim"
