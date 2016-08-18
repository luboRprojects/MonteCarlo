library(dplyr)
source("C:\\Users\\homolka\\Desktop\\MonteCarlo\\Scripts\\import_assumptions.R")
source("C:\\Users\\homolka\\Desktop\\MonteCarlo\\Scripts\\compute_samples.R")

mc.samples <- compute_samples()

mc_df_1 <- mc.samples[[1]]
# colnames(mc_df_1)
mc_df_2 <- mc.samples[[2]]
n.samples <- mc.samples[[3]]

#-------- Start looping -------

# for(i in 1:n.samples){ # commented for DEV only

# Revenues - quantity growth

g1_q <- c(rep(NA,5))
g2_q <- c(rep(NA,5))
quant_growth <- as.matrix(rbind(g1_q, g2_q, 
 as.numeric(mc_df_1[i,c("q_growthFY15M1","q_growthFY16M1","q_growthFY17M1","q_growthFY18M1","q_growthFY19M1")]),
 as.numeric(mc_df_1[i,c("q_growthFY15M2","q_growthFY16M2","q_growthFY17M2","q_growthFY18M2","q_growthFY19M2")]),
 as.numeric(mc_df_1[i,c("q_growthFY15M3","q_growthFY16M3","q_growthFY17M3","q_growthFY18M3","q_growthFY19M3")])) )

# Revenues - prices growth

g1_p <- c(rep(NA,5))
g2_p <- c(rep(NA,5))

price_growth <- as.matrix(rbind(g1_p, g2_p, 
 as.numeric(mc_df_1[i,c("p_growthFY15M1","p_growthFY16M1","p_growthFY17M1","p_growthFY18M1","p_growthFY19M1")]),
 as.numeric(mc_df_1[i,c("p_growthFY15M2","p_growthFY16M2","p_growthFY17M2","p_growthFY18M2","p_growthFY19M2")]),
 as.numeric(mc_df_1[i,c("p_growthFY15M3","p_growthFY16M3","p_growthFY17M3","p_growthFY18M3","p_growthFY19M3")])) )

rownames(quant_growth ) <- rownames(price_growth) <- NULL
# price_growth
# quant_growth

# Change in proportion of material costs on total cost
perc_cost_mat <- c(-0.10, 0.05, 0.05, 0.05, 0.1) # proportion in years 2015-2019

# Change in labor costs
sal.incr <- c(0.05, 0.05, 0.05, 0.05, 0.1) # proportion in years 2015-2019

# } # Close loop
