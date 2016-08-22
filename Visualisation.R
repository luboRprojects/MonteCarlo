library(ggplot2)
library(scales)
library(lubridate)

data.in <- read.csv("DHA_prices.txt", sep="\t")
gg.data <- data.frame(FCFF = FCFF_foo, 
 inside = ifelse(quantile(FCFF_foo,0.95)<FCFF_foo | FCFF_foo<quantile(FCFF_foo,0.05), "out", "in") )

ggplot(gg.data, aes(x=FCFF, fill=inside) ) + geom_histogram(bins=100, colour="darkblue") + 
scale_x_continuous(labels = comma) + 
theme_bw()

quantile(FCFF_foo,probs =c(0.05, 0.95) )



