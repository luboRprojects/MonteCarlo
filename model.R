library(dplyr)
source("C:\\Users\\homolka\\Desktop\\MonteCarlo\\Scripts\\import_assumptions.R")
source("C:\\Users\\homolka\\Desktop\\MonteCarlo\\Scripts\\compute_samples.R")

mc.samples <- compute_samples()

mc_df_1 <- mc.samples[[1]]
# colnames(mc_df_1)
mc_df_2 <- mc.samples[[2]]
n.samples <- mc.samples[[3]]

#-------- Start looping -------
i=1
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
# quant_growth
# price_growth

# Change in proportion of material costs on total cost
perc_cost_mat <- as.numeric(mc_df_2[i,c("mat_costFY15","mat_costFY16","mat_costFY17","mat_costFY18","mat_costFY19")])

as.numeric(mc_df_2[i,c("proc_costFY15","proc_costFY16","proc_costFY17","proc_costFY18","proc_costFY19")])
# Change in labor costs
sal.incr <- as.numeric(mc_df_2[i,c("lab_costFY15","lab_costFY16","lab_costFY17","lab_costFY18","lab_costFY19")])

# Change in general processing costs
gr.tab_gen_proc <- 1+matrix(c(

# } # Close loop
