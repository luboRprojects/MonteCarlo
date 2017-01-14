library(ggplot2)
library(scales)
library(lubridate)

data.in <- read.csv("DHA_prices.txt", sep="\t")
data.ts <- data.in
data.ts$PX.LAST <- dmy(data.ts$PX.LAST)
gg.data <- data.frame(FCFF = FCFF_foo, 
 inside = ifelse(quantile(FCFF_foo,0.95)<FCFF_foo | FCFF_foo<quantile(FCFF_foo,0.05), "out", "in") )
gg.data$FCFF <- gg.data$FCFF/15119946*1000000


ggplot(gg.data, aes(x=FCFF, fill=inside) ) + 
 geom_histogram(bins=60, colour="darkblue") + 
scale_x_continuous(labels = comma) + 
theme_bw() + guides(fill=FALSE) + 
 ggtitle("Monte Carlo Target Value") + 
 geom_vline(xintercept = 28446)


data2016 <- data.ts %>% filter(PX.LAST > dmy("31-12-2015"))
data2016$lo70 <- quantile(gg.data$FCFF, 0.70)
data2016$up70 <- quantile(gg.data$FCFF, 0.30)
data2016$lo90 <- quantile(gg.data$FCFF, 0.90)
data2016$up90 <- quantile(gg.data$FCFF, 0.10)
data2016$est <- quantile(gg.data$FCFF, 0.50)

l0 <- ggplot(data.ts, aes(x=PX.LAST, y=DHA.VM.Equity)) + 
 geom_line(colour="darkblue", size=1.05) + 
 scale_y_continuous(
  "DHA share value",
  limits=c(0, 1.05*max(data.in$DHA.VM.Equity)),
  labels = comma)+
 theme_bw()

l0 + geom_line(data=data2016, aes(PX.LAST, y=est), colour="red", size=1.5 ) + 
 geom_ribbon(data=data2016, aes(ymin=lo90, ymax=up90), fill="blue", alpha=0.15) + 
 geom_ribbon(data=data2016, aes(ymin=lo70, ymax=up70), fill="red", alpha=0.15)  + 
 scale_x_date(name="Date")






