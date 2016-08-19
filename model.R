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

# Change in labor costs
sal.incr <- as.numeric(mc_df_2[i,c("lab_costFY15","lab_costFY16","lab_costFY17","lab_costFY18","lab_costFY19")])

# Change in general processing costs
gr.tab_gen_proc <- 1 + as.matrix(rbind(g1_p, g2_p,
 as.numeric(mc_df_1[i,c("proc_costFY15M1","proc_costFY16M1","proc_costFY17M1","proc_costFY18M1","proc_costFY19M1")]),
 as.numeric(mc_df_1[i,c("proc_costFY15M2","proc_costFY16M2","proc_costFY17M2","proc_costFY18M2","proc_costFY19M2")]),
 as.numeric(mc_df_1[i,c("proc_costFY15M3","proc_costFY16M3","proc_costFY17M3","proc_costFY18M3","proc_costFY19M3")]) )
)

# Selling percent - multiplied by revenues
# to get selling expenditures.
selling_perc <- rep((0.18788-0.01)/100, 5)

selling_perc <- mc_df_2[i,c("sell_expFY15", "sell_expFY16", "sell_expFY17", "sell_expFY18", "sell_expFY19")]

# Administrative costs - multiplied by revenues
admin_perc <- mc_df_2[i,c("admin_expFY15", "admin_expFY16", "admin_expFY17", "admin_expFY18", "admin_expFY19")]

# Interest on loans
int_loans <- mc_df_2[i,c("loans_inFY15", "loans_inFY16", "loans_inFY17", "loans_inFY18", "loans_inFY19")]

# Allocation to Investment and development fund
inv_dev_perc <- mc_df_2[i,c("invdev_fundFY15", "invdev_fundFY16", "invdev_fundFY17", "invdev_fundFY18", "invdev_fundFY19")]

# Allocation to Financial provision fund
fin_prov_perc <- mc_df_2[i,c("fin_provFY15", "fin_provFY16", "fin_provFY17", "fin_provFY18", "fin_provFY19")]

# Estimated dividend per share 
div_per_share <- mc_df_2[i,c("div_per_shareFY15", "div_per_shareFY16", "div_per_shareFY17", "div_per_shareFY18", "div_per_shareFY19")]

#----------- Revenues ------------- 
quantity_0 <- c(95400, 269490,  195898,  322077,  704218)
sell_price_0 <- c(91500, 83000,  134578,  114929,  108794)
rev_init <- c(8206, 3648, 27286, 37757, 129710)

quantity0 <- data.frame(init = quantity_0, 1 + quant_growth)
quantity <- apply(quantity0, 1, function(x){cumprod(as.numeric(x))})

prices0 <- data.frame(init = sell_price_0, 1 + price_growth)
prices <- apply(prices0, 1, function(x){cumprod(as.numeric(x))})
prices[ ,5] <- c(108794, 113000, 120000, 127000, 134000, 140700) 

rev_tab0 <- revenues <- quantity * prices / 1000000
rev_tab0 <- rev_tab0[-1, ]
rev_tab0[1, 1:2] <- c(8206, 3648)

revenues <- rowSums(data.frame(rev_tab0), na.rm=TRUE)
# Keep 0.208 and 9679 - rely on 2014 data
sales_growth <- c(0.20758, diff(revenues)/revenues[1:length(diff(revenues))])

#-------------- COGS --------------
#--- Raw Material ------

cost_raw_mat0 <- c(NA, NA, 1734, 2850, 6232) #initial costs
 cost_raw_mat1 <- matrix(cost_raw_mat0, ncol=1)
 cost_raw_mat1[1:2, ] <- 0

temp.growth <- matrix(rep((1+perc_cost_mat),5), ncol=5, byrow=TRUE)
temp.growth[5, ] <- 1

matcost_15 <- cost_raw_mat1 * (1+c(rep((perc_cost_mat)[1],4),0)) * (1+quant_growth[,1])
matcost_16 <- matcost_15 * (1+c(rep((perc_cost_mat)[2],4),0)) * (1+quant_growth[,2])
matcost_17 <- matcost_16 * (1+c(rep((perc_cost_mat)[2],4),0)) * (1+quant_growth[,3])
matcost_18 <- matcost_17 * (1+c(rep((perc_cost_mat)[2],4),0)) * (1+quant_growth[,4])
matcost_19 <- matcost_18 * (1+c(rep((perc_cost_mat)[2],4),0)) * (1+quant_growth[,5])

cost_mat0 <- data.frame(matcost_15, matcost_16, matcost_17, matcost_18, matcost_19)
cost_raw_mat <- as.numeric(colSums(cost_mat0, na.rm=TRUE))

#--- Labour cost ------
cost_lab0 <- c(NA, NA,  1733, 2849, 6229) #initial costs

t1 <- (1+matrix(rep(sal.incr, 5), byrow=TRUE, nrow=5)) * (1+quant_growth)
dd <- data.frame(init=cost_lab0, t1)
cost_lab0 <- apply(dd, 1, function(x){cumprod(as.numeric(x))})[-1, ]

cost_lab <- rowSums(cost_lab0, na.rm=TRUE)
deprec0 <-  c(9048, 9048,9048, 9048, 9048) #12*some constant...

#--- General processing costs ------
cost_gen_proc0 <- c(NA, NA, 12320, 20255, 44287) #initial costs

temp.gpd <- gr.tab_gen_proc * (1 + quant_growth)
 rownames(temp.gpd) <- NULL

gen_proc_growth <- data.frame(init = cost_gen_proc0, temp.gpd )
quantity_gen_proc <- apply(gen_proc_growth, 1, function(x){cumprod(as.numeric(x))})[-1, ]

cost_gen_proc <- rowSums(quantity_gen_proc, na.rm=TRUE)

capacity <- c(NA, NA, 280000, 490000, 650000)
cap_constant <- c(NA, NA, 11, 10, 6)
royal <- 12500

#--------- Constants -----
K_fact <- 0.9
grant_money <- c(0.69, 0.7, 0.68, 0.66 ,0.65)

min_right_const <- royal * K_fact * grant_money

cost_min_right <- capacity * cap_constant * min_right_const / 1E6
cost_min_right[which(is.na(cost_min_right))] <- 0
cost_min_right <- rep(sum(cost_min_right), 5)


colnames(mc_df_1)
colnames(mc_df_2)
# } # Close loop
