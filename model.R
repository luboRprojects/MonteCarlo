library(dplyr)
source("C:\\Users\\homolka\\Desktop\\MonteCarlo\\Scripts\\import_assumptions.R")
source("C:\\Users\\homolka\\Desktop\\MonteCarlo\\Scripts\\compute_samples.R")

mc.samples <- compute_samples()

mc_df_1 <- mc.samples[[1]]
mc_df_2 <- mc.samples[[2]]
