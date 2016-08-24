library(dplyr)
library(tidyr)
library(ggplot2)

str(revenues_foo)
colnames(revenues_foo) <- paste0("FY", 15:19)
revenues_foo$id <- rownames(revenues_foo)
gg.data <- gather(revenues_foo, year, id) 
 colnames(gg.data)[3] <- "value"

ggplot(gg.data, aes(x=year, y=value, group=id) ) + 
 geom_line(alpha=0.1)