library(dplyr)
#--- Settings ----
n_iter <- 2
#--- Constants ---
shares <- rep(15119946, 5)
inc_tax_rate <- rep(0.2, 5)

#=========== Assumptions =========== 
# Revenues - quantity growth
g1_q <- c(rep(NA,5))
g2_q <- c(rep(NA,5))
g3_q <- c(-0.1, 0.05, 0.05, 0.05, 0.05)
g4_q <- c(0, 0.15, 0.15, 0.15, 0.15)
g5_q <- c(0.63, -0.15, 0.2, 0.15, 0.1)
quant_growth <- matrix(rbind(g1_q, g2_q, g3_q, g4_q, g5_q), nrow=5 )

# Revenues - prices growth
g1_p <- c(rep(NA,5))
g2_p <- c(rep(NA,5))
g3_p <- c(0.15, 0.05, 0.05, 0.05, 0.05)
g4_p <- c(0.02, 0.07, 0.05, 0.05, 0.05)
g5_p <- c(0.039, 0.062, 0.058, 0.55, 0.05)
price_growth <- matrix(rbind(g1_p, g2_p, g3_p, g4_p, g5_p), nrow=5 )

# Change in proportion of material costs on total cost
perc_cost_mat <- c(-0.10, 0.05, 0.05, 0.05, 0.1) # proportion in years 2015-2019

# Change in labor costs
sal.incr <- c(0.05, 0.05, 0.05, 0.05, 0.1) # proportion in years 2015-2019

# Change in general processing costs
gr.tab_gen_proc <- 1+matrix(c(
 NA, NA, NA, NA, NA,
 NA, NA, NA, NA, NA,
 0.01, 0.05, 0.05, 0.05, 0.05,
 0.06, 0.06, 0.06, 0.06, 0.06,
 0.01, 0.01, 0.01, 0.01, 0.01), ncol=5, byrow=TRUE)

# Selling percent - multiplied by revenues
# to get selling expenditures.
selling_perc <- rep((0.18788-0.01)/100, 5)

# Administrative costs - multiplied by revenues
admin_perc <- rep((6.17119+0.1)/100, 5)

# Interest on loans
int_loans <- rep(0.08, 5)

# Allocation to Investment and development fund
inv_dev_perc <- rep(0.1, 5)

# Allocation to Financial provision fund
fin_prov_perc <- rep(0.05, 5)

# Estimated dividend per share 
div_per_share <- c(1000, 1200, 1500, 1500, 2000)

#==================================
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

#==================================
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

gen_proc_growth <- data.frame(init = cost_gen_proc0, gr.tab_gen_proc * (1 + quant_growth) )
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
#==============================

#----------------------------- TODO -----------------------------

##################### OWN EQUITY #####################
 paid_up_cap <- rep(151199, 5)
owners_equity <- paid_up_cap
#------
 shar_prem <- rep(58398, 5)
share_premium <- shar_prem
#------
 tres_share <- rep(-1298, 5)
treasury_shares <- tres_share
#------

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
# Keep 0.208 and 9679 - rely on 2014 data
# sales_growth <- c(0.20758, diff(revenues)/revenues[1:length(diff(revenues))])
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

in_tax <- prof_bt * inc_tax_rate 
net_profit_after_tax <- prof_bt - in_tax

#============ Equity =============
inv_dev_fund <- cumsum(c(73695 , net_profit_after_tax * inv_dev_perc))[-1]
#------
fin_provisions <- cumsum(c(15100, net_profit_after_tax * fin_prov_perc))[-1]
#------
welfare_fund_perc <- rep(0, 5)
div_per_share <- c(1000, 1200, 1500, 1500, 2000)
div_policy <- shares* div_per_share / 1000000	

 ret_earnings <- cumsum(c(8437,
   net_profit_after_tax * (1-(inv_dev_perc + fin_prov_perc + welfare_fund_perc))- 
   div_policy ))[-1] 

#-------------- 
 capital_reserves <- owners_equity + share_premium + treasury_shares + 
  inv_dev_fund + fin_provisions + ret_earnings

#=====

 owners_equity <- capital_reserves

total_assets <- owners_equity + liabilities

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

wacc_short <- data.frame(
 matrix(rep(rep(0.1686, 5),n_iter),
 ncol=5, byrow=TRUE )
)

disc_fact <- data.frame(t(apply((1+wacc_short), 1, cumprod)))

FCFF <- data.frame(
 matrix(rep(
  c(32346.8711735, 22356.6509809, 23510.3494305, 32840.7445284, 48246.0168763),n_iter),
 ncol=5, byrow=TRUE ))

#-----
growth_long <- data.frame(matrix(rep(0.02, n_iter), nrow=1))
wacc_long <- data.frame(matrix(rep(0.09842905, n_iter), nrow=1))
#-----


pv.cf <- data.frame(
 iter = 1:n.iter,
 pv.cf = apply(FCFF / disc_fact, 1, sum),
 terminal = t(( FCFF[ ,ncol(FCFF)] * (1+growth_long) ) / 
  (wacc_long - growth_long ) / 
  disc_fact[ ,ncol(disc_fact)])
) %>% mutate(Value1 = pv_cf + terminal)











############# DEV ##############
colnames(FCFF) <- colnames(disc_fact) <- paste0("FY",2015:2019)
#--- DHA Monte Carlo ----

disc_factor <- cumprod(1+wacc_short)
pv_cf_sum <- sum(FCF/disc_factor)
 pv_terminal <- 
  ( last(FCF) * (1+growth_long) ) / 
  (wacc_long - growth_long) / 
  last(disc_factor)

pv_eq <- pv_cf_sum + pv_terminal
FCFE_target <- pv_eq * shares






