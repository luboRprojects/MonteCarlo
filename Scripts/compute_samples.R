compute_samples <- function(){

#---- Assumptions ----
assumptions <- import_assumptions()

a1_prob <- grep(x=assumptions[[1]]$Variable, pattern="_P")
a1_vals <- which(!(1:nrow(assumptions[[1]]) %in% a1_prob))

a1 <- data.frame(assumptions[[1]][a1_vals, ], assumptions[[1]][a1_prob, ])
a2 <- data.frame(assumptions[[2]][a1_vals, ], assumptions[[2]][a1_prob, ])
#---- Constants -----
# TODO: read from external file
shares <- rep(15119946, 5)
inc_tax_rate <- rep(0.2, 5)

quantity_0 <- c(95400, 269490,  195898,  322077,  704218)
sell_price_0 <- c(91500, 83000,  134578,  114929,  108794)
rev_init <- c(8206, 3648, 27286, 37757, 129710)

unique(assumptions[[1]]$Variable)

#---- Compute Simulated values - Mines & General ----
n.boot <- 50

a1.2 <- a1 %>% group_by(Mine, Year, Variable) %>% select(Value, Value.1) %>%
 mutate(
  class = as.factor(paste0(Variable, Year, Mine)),
  prob = Value.1/100
  ) %>% ungroup() %>% split(.,.$class)

a2.2 <- a1 %>% group_by(Year, Variable) %>% select(Value, Value.1) %>%
 mutate(
  class = as.factor(paste0(Variable, Year)),
  prob = Value.1/100
  ) %>% ungroup() %>% split(.,.$class)

mc_values.1 <- lapply(a1.2, function(x){
 sample(x$Value, size=n.boot, replace=TRUE, prob=x$prob)
 })

mc_values.2 <- lapply(a2.2, function(x){
 sample(x$Value, size=n.boot, replace=TRUE, prob=x$prob)
 })

mc_df_1 <- do.call(cbind.data.frame, mc_values.1)
mc_df_2 <- do.call(cbind.data.frame, mc_values.2)

return(list(mc_df_1, mc_df_2)) 
}


