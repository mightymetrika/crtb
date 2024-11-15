## code to prepare `crtb_lm_sim_runtimes` dataset goes here
library(readxl)
crtb_lm_sim_runtimes <- readxl::read_xlsx("rawdatafiles/crtb_lm_sim_runtimes.xlsx")

usethis::use_data(crtb_lm_sim_runtimes, overwrite = TRUE)
