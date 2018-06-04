#################
#Testing Random Walk Hypothesis #
######################
### Tests included:
####### CJ test
####### Runs Test

if(!require("randtests")) install.packages("randtests"); library("randtests")




files <- list.files(path="data/", pattern="*.csv", full.names=T, recursive=FALSE)
files <- as.vector(files)

test_table <- data.frame(row.names = 1:30)
test_table$data <- files
test_table$CJ <- 0



# CJ test ##########


for (i in files){
  
  series <- read.csv(i)
  
  
  series$Open <- as.character(series$Open)
  series$Open <- as.numeric(series$Open)
  
  series$returns <- 0
  series <- na.omit(series)
  
  last_row <- nrow(series)
  row.names(series) <- seq(1:last_row)
  
  return_row <- nrow(series) - 1
  
  for (j in 1:return_row){
    series$returns[j+1] <- (log(series$Open[j+1]) - log(series$Open[j]))
  }
  
  series$indi <- 0
  series$indi <- ifelse(series$returns < 0, 0, 1)
  
  series <- na.omit(series)
  sum(series$indi)/length(series$indi)
  
  
  series$y <- 0
  for (k in 1:length(series$Open)){
    series$y[k] <- (series$indi[k]*series$indi[k+1]) + (1 - series$indi[k])*(1 - series$indi[k+1]) 
  }
  
  series <- na.omit(series)
  no_seq <- sum(series$y)
  
  CJ_stat <- no_seq/(length(series$returns) - no_seq)
  
  pi<- sum(series$indi)/length(series$indi)
  pi_s <- pi^2 + (1- pi)^2
  
  
  
  
  asy_varCJ <- (pi_s * (1 - pi_s) + 2*(pi ^ 3 + (1- pi) ^ 3 - pi_s^2))/(length(series$returns)*(1 - pi_s)^4)
  
  
  t_stat <-  (CJ_stat - (pi_s/(1 - pi_s)))/sqrt(asy_varCJ) 
  
  
  test_table$CJ[test_table$data == i] <- 2*pnorm(-abs(t_stat))
  
  
  
}



# Runs Test -------------


test_table$Runs <- 0

for (i in files){
  
  series <- read.csv(i)
  
  
  series$Open <- as.character(series$Open)
  series$Open <- as.numeric(series$Open)
  
  series$returns <- 0
  series <- na.omit(series)
  
  last_row <- nrow(series)
  row.names(series) <- seq(1:last_row)
  
  return_row <- nrow(series) - 1
  
  for (j in 1:return_row){
    series$returns[j+1] <- (log(series$Open[j+1]) - log(series$Open[j]))
  }
  
  RT <-   runs.test(x = series$returns, plot= F)
  
  test_table$Runs[test_table$data == i] <- RT$p.value
  
}  

# ACF tests ------
# Box*Pierce & Ljung-Box tests

for (i in files){
  
  series <- read.csv(i)
  
  
  series$Open <- as.character(series$Open)
  series$Open <- as.numeric(series$Open)
  
  series$returns <- 0
  series <- na.omit(series)
  
  last_row <- nrow(series)
  row.names(series) <- seq(1:last_row)
  
  return_row <- nrow(series) - 1
  
  for (j in 1:return_row){
    series$returns[j+1] <- (log(series$Open[j+1]) - log(series$Open[j]))
  }
  
  
  
  
  bp_name <- paste("bpTable_", i, sep = "")
  lb_name <- paste("lbTable_", i, sep = "")
  
  lags <- c(1,2,3,4,5,6, 7, 10, 14, 30, 90, 365)
  bp_table <- data.frame(row.names = 1:length(lags))
  bp_table$lags <- lags
  lb_table <- data.frame(row.names = 1:length(lags))
  lb_table$lags <- lags
  
  
  
  for (m in lags){
    
    bp <- Box.test(series$returns, lag = m, type = "Box-Pierce", fitdf = 0)
    bp_table$pvalue[bp_table$lags == m] <- bp$p.value 
    assign(x = bp_name, value = bp_table)
    
    
    lb <- Box.test(series$returns, lag = m, type = "Ljung-Box", fitdf = 0)
    lb_table$pvalue[lb_table$lags == m] <- lb$p.value 
    assign(x = lb_name, value = lb_table)
    
    export_bp <- paste("data/", bp_name, sep = "")
    write.csv(bp_table, export_bp)
    
    export_lb <- paste("data/", lb_name, sep = "")
    write.csv(lb_table, export_lb)
    
    
  }
  
  
}


