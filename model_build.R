library(dplyr)
source("C:\\Users\\homolka\\Desktop\\MonteCarlo\\Scripts\\import_assumptions.R")

#---- Assumptions ----
assumptions <- import_assumptions()

a1_prob <- grep(x=assumptions[[1]]$Variable, pattern="_P")
a1_vals <- which(!(1:nrow(assumptions[[1]]) %in% a1_prob))

a1 <- data.frame(assumptions[[1]][a1_vals, ], assumptions[[1]][a1_prob, ])
#---- Constants -----
# TODO: read from external file
shares <- rep(15119946, 5)
inc_tax_rate <- rep(0.2, 5)

quantity_0 <- c(95400, 269490,  195898,  322077,  704218)
sell_price_0 <- c(91500, 83000,  134578,  114929,  108794)
rev_init <- c(8206, 3648, 27286, 37757, 129710)

unique(assumptions[[1]]$Variable)