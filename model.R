library(dplyr)
source("C:\\Users\\homolka\\Desktop\\MonteCarlo\\Scripts\\import_assumptions.R")
source("C:\\Users\\homolka\\Desktop\\MonteCarlo\\Scripts\\compute_samples.R")

mc.samples <- compute_samples()

mc_df_1 <- mc.samples[[1]]
# colnames(mc_df_1)
mc_df_2 <- mc.samples[[2]]
n.samples <- mc.samples[[3]]

FCFF_foo <- c()
revenues_foo <- data.frame(matrix(1, ncol=5))
net_profit_after_tax_foo <- data.frame(matrix(1, ncol=5))
COGS_foo <- data.frame(matrix(1, ncol=5)) 

#-------- Start looping -------
#i=n.samples=1
 for(i in 1:n.samples){ # commented for DEV only

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
quantity_0 <- c(95400, 269490, 195898, 322077, 704218)
sell_price_0 <- c(91500, 83000, 134578, 114929, 108794)
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
#---------- Raw Material ----------

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

#---------- Labour cost ----------
cost_lab0 <- c(NA, NA,  1733, 2849, 6229) #initial costs

t1 <- (1+matrix(rep(sal.incr, 5), byrow=TRUE, nrow=5)) * (1+quant_growth)
dd <- data.frame(init=cost_lab0, t1)
cost_lab0 <- apply(dd, 1, function(x){cumprod(as.numeric(x))})[-1, ]

cost_lab <- rowSums(cost_lab0, na.rm=TRUE)
deprec0 <-  c(9048, 9048,9048, 9048, 9048) #12*some constant...

#---- General processing costs ----
cost_gen_proc0 <- c(NA, NA, 12320, 20255, 44287) #initial costs

temp.gpd <- gr.tab_gen_proc * (1 + quant_growth)
 rownames(temp.gpd) <- NULL

gen_proc_growth <- data.frame(init = cost_gen_proc0, temp.gpd )
quantity_gen_proc <- apply(gen_proc_growth, 1, function(x){cumprod(as.numeric(x))})[-1, ]

cost_gen_proc <- rowSums(quantity_gen_proc, na.rm=TRUE)

capacity <- c(NA, NA, 280000, 490000, 650000)
cap_constant <- c(NA, NA, 11, 10, 6)
royal <- 12500

#--------- Constants ---------
K_fact <- 0.9
grant_money <- c(0.69, 0.7, 0.68, 0.66 ,0.65)

min_right_const <- royal * K_fact * grant_money

# TODO-------------------
# cost_min_right <- capacity * cap_constant * min_right_const / 1E6
# cost_min_right[which(is.na(cost_min_right))] <- 0
# cost_min_right <- rep(sum(cost_min_right), 5)
cost_min_right <- rep(6683, 5)

#========
cogs <- cost_raw_mat + cost_lab + deprec0 + cost_gen_proc + cost_min_right
#======== Net Income after taxes =========
# net_in_at

prof_gr <- revenues - cogs

#------
sell_exp <- (-revenues) * selling_perc 
#------
gen_admin_exp <- (-revenues) * admin_perc
#------
EBITDA <- prof_gr + sell_exp + gen_admin_exp
 deprec <-  c(9694, 10694, 10694, 10694, 10694)  # should use this one - TODO: DECOMPOSE!
EBIT <- EBITDA - deprec


######################################################
######################################################
##################### OWN EQUITY #####################
 paid_up_cap <- rep(151199, 5)
owners_equity <- paid_up_cap
#------
 shar_prem <- rep(58398, 5)
share_premium <- shar_prem
#------
 tres_share <- rep(-1298, 5)
treasury_shares <- tres_share

#============ Liabilities =============
  st_borrowings <- cumprod(c(17520, 1+sales_growth))[-1]
#---
   days_payable <- rep(18, rep=5)
  trade_acc_pay <- days_payable * cogs/365
#---
  cust_advanc <- cumprod(c(1140, 1+sales_growth))[-1]
#---
  taxes_payable <- cumprod(c(1916, 1+sales_growth))[-1]
#---
  empl_payable <- cumprod(c(3533, 1+sales_growth))[-1]
#---
  acc_exp <- cumprod(c(6449, 1+sales_growth))[-1] 
#---
  other_payb <- cumprod(c(1134, 1+sales_growth))[-1] 
#---
 current_liab <- st_borrowings + trade_acc_pay + cust_advanc + 
  taxes_payable + empl_payable + acc_exp + other_payb

#=====
liabilities <- current_liab

#==============

short_term_inv <- rep(23814, 5)
#------
 days.receiv <- rep(70, 5)
 receiv.constant <- rep(13000,5) #!
account_receivables <- revenues * days.receiv/365 + receiv.constant
#------
 days.invent <- rep(41, 5)
inventories <- cogs * days.invent / 365
#------
other_curr_assets <- 9679 * cumprod(1+sales_growth)
#-----------------
sht_0 <- short_term_inv + account_receivables + 
 inventories + other_curr_assets
#=========================================================
# Need to compute cash and cash equivalents computed as:
# Cash = Total Assets - sht_0 - long_term_assets
#----------------------------
# tf_costs are "computed" in non-standard way; set constants
   tf_cost <- cumsum(c(83962, 4267, 27828, 29193, 34193, 32828))[-1]
   tang_accum.dep <- cumsum(c(45348, deprec))[-1]
  tang_fixed <- tf_cost - tang_accum.dep
#---------
  intang_fixed <- c(101833, 101433, 101033, 100633, 100233) 
#---------
  constr_in_prog <- c(37896, 65724, 94918, 129111, 161940) 
#=====
 fixed_assets <- tang_fixed + intang_fixed + constr_in_prog
#
  unlisted_stocks <- rep(45146, 5)
#---------
  int_joint_ventures <- rep(8459, 5)
#---------
 long_investment <- unlisted_stocks + int_joint_ventures
#---------
# Non-systematic -> autofill

load_land_cove <- data.frame(matrix(c(
 NA, NA, 464, 3131, 1430,
 NA, NA, 1363, 7200, 2560,
 NA, NA, 1449, 7652, 2816,
 NA, NA, 1630, 8609, 2816,
 NA, NA, 1778, 9392, 2688),nrow=5, ncol=5) )

 other_long_inv <- colSums(load_land_cove, na.rm=TRUE)
long_term_assets <- fixed_assets + long_investment + other_long_inv


####################### NEED OF NET PROFIT #####################
#################################################################
# Financial Income was imputed from Excel to avoid circular

#============================
#dep_rate <- rep(0.05, 5)
# int_dep_bank <- (cash + short_term_inv) * dep_rate 

# Non-standard way:
# other_fin_income <- c(7762.415,  11067, 13601, 16274, 18890) 

fin_income <-  c(11003, 12819, 14201, 15948, 18126) 

#-------
interest_expenses <- st_borrowings * int_loans
fin_exp_others <- rep(0, 5)
fin_expenses <- interest_expenses + fin_exp_others
#-------

prof_bt <- prof_gr + sell_exp + gen_admin_exp + fin_income - fin_expenses

inc_tax_rate <- rep(0.2, 5)
in_tax <- prof_bt * inc_tax_rate 
net_profit_after_tax <- as.numeric(prof_bt - in_tax)

#============ Equity =============
inv_dev_fund <- cumsum(c(73695 , net_profit_after_tax * inv_dev_perc))[-1]
#------
fin_provisions <- cumsum(c(15100, net_profit_after_tax * fin_prov_perc))[-1]
#------

welfare_fund_perc <- rep(0, 5)
shares <- rep(15119946, 5)
div_policy <- shares*div_per_share / 1000000	

 ret_earnings <- cumsum(c(8437,
   net_profit_after_tax * (1-(inv_dev_perc + fin_prov_perc + welfare_fund_perc))- 
   div_policy ))[-1] 

#-------------- 
 capital_reserves <- owners_equity + share_premium + treasury_shares + 
  inv_dev_fund + fin_provisions + ret_earnings

#=====
 owners_equity <- capital_reserves

total_assets <- as.numeric(owners_equity + liabilities)

#-----------------
#total_assets <- sht_0 + cash + long_term_assets
cash <- total_assets - sht_0 - long_term_assets

#==================================================
#--------------------  FCFF  ----------------------

net_in_at <- net_profit_after_tax # Net Income after taxes

#=====================================================
 delta_receiv <- diff(c(47635, account_receivables)) * (-1)
 delta_invent <- diff(c(3887, inventories)) * (-1)
 delta_payable <- diff(c(17758, trade_acc_pay + cust_advanc + 
  taxes_payable + empl_payable + acc_exp + other_payb) )
 delta_other_assets <- diff(c(9679, other_curr_assets) ) * (-1)

net_working_cap <-  delta_receiv + delta_invent + 
  delta_payable +  delta_other_assets

#=====================================================
CFO <- net_in_at + deprec + net_working_cap 
int_exp_at <- (-1) * interest_expenses * (1-inc_tax_rate)

# Different computation
net_invest_fix_cap <- c(4267, 27828, 29193, 34193, 32828) 

FCFF <- CFO +  int_exp_at - net_invest_fix_cap

wacc_short <- mc_df_2[i,c("wacc_shortFY15", "wacc_shortFY16", "wacc_shortFY17", "wacc_shortFY18", "wacc_shortFY19")]
disc_fact <- as.numeric(t(apply((1+wacc_short), 1, cumprod)))

#-----
growth_long <- mc_df_2[i,"growth_longFY20"]
wacc_long <- mc_df_2[i,"wacc_longFY20"]
#-----

pv.cf <- data.frame(
 pv.cf = apply(FCFF / disc_fact, 1, sum),
 terminal = last(FCFF) * (1+growth_long) /
  (wacc_long - growth_long ) /
  disc_fact[length(disc_fact)]) %>%
 mutate(FCFF = pv.cf + terminal)

FCFF_foo[i] <- pv.cf$FCFF
revenues_foo[i] <- revenues
net_profit_after_tax_foo[i] <- net_profit_after_tax
COGS_foo[i] <- cogs

}


#=========== DEV ===========
library(ggplot2)

gg.data <- data.frame(FCFF = FCFF_foo, 
 inside = ifelse(quantile(FCFF_foo,0.95)<FCFF_foo | FCFF_foo<quantile(FCFF_foo,0.05), "out", "in") )

FCFF_foo>35000

hist(FCFF_foo)


#colnames(mc_df_1)
#colnames(mc_df_2)
