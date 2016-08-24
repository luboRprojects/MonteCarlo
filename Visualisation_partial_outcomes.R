library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#------------- Revenues -------------
colnames(revenues_foo) <- paste0("FY", 15:19)
revenues_foo$id <- rownames(revenues_foo)
gg.data <- gather(revenues_foo, year, id) 
 colnames(gg.data)[3] <- "value"

ggplot(gg.data, aes(x=year, y=value, group=id) ) + 
 geom_line(alpha=0.05, colour="darkblue") + 
 scale_y_continuous(labels = comma) + theme_bw()

#------------- COGS -------------
colnames(COGS_foo) <- paste0("FY", 15:19)
COGS_foo$id <- rownames(COGS_foo)
gg.data <- gather(COGS_foo, year, id)
 colnames(gg.data)[3] <- "value"

ggplot(gg.data, aes(x=year, y=value, group=id) ) + 
 geom_line(alpha=0.05, colour="darkblue") + 
 scale_y_continuous(labels = comma) + 
 ggtitle("Cost of Goods Sold") + 
 theme_bw()

#------------- FCFE -------------
colnames(fcff_annual_foo) <- paste0("FY", 15:19)
fcff_annual_foo$id <- rownames(fcff_annual_foo)
gg.data <- gather(fcff_annual_foo, year, id)
 colnames(gg.data)[3] <- "value"

ggplot(gg.data, aes(x=year, y=value, group=id) ) + 
 geom_line(alpha=0.05, colour="darkblue") + 
 scale_y_continuous(labels = comma) + theme_bw()
fcff_annual_foo