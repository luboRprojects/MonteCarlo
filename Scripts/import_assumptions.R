import_assumptions <- function(ticker="DHA"){ 
setwd("C:/Users/homolka/Desktop/MonteCarlo/Data")
# ticket = add of ticker in capital letters "HAG"

file1 <- paste0("mc1_", ticker, ".txt")
file2 <- paste0("mc2_", ticker, ".txt")

assump1 <- read.csv(file1, sep="\t")
assump2 <- read.csv(file2, sep="\t")

foo <- list(assump1, assump2)

return(foo) 


}