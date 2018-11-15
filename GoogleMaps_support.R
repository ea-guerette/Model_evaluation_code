#this is the code to help make GoogleMapsPlot work 

library(openair)

strip = function(...) strip.default(...)
strip.left = strip.custom(style=1, horizontal = F)

col_bt <- rev(openColours("Reds", 50))
col_t <- "white"
col_at <- openColours("Blues", 50)
colBubble <- c(col_bt, col_t, col_at) 


BubbleKey <- function(df, pollutant, threshold) {
breaks_bt <- seq(min(df[[pollutant]], na.rm = T), threshold, length = 51)
breaks_at <- seq(threshold, max(df[[pollutant]], na.rm = T), length = 51)
breaks <- c(breaks_bt,breaks_at[-1])
#temp <- range(breaks)  
#mycols <- mycols <- ifelse(df[[pollutant]] <= temp[2] & df[[pollutant]] >= temp[1], df[[pollutant]], NA)

#mycols <- colBubble[cut(mycols, c(breaks[1] - 1, breaks), labels = FALSE)]
mykey_b <- list(col = colBubble, at = breaks) 

return(mykey_b)
}

