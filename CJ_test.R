series <- read.csv("data/ADS.DE.csv")


series$Open <- as.character(series$Open)
series$Open <- as.numeric(series$Open)

series$returns <- 0

for (i in 1:length(series$Date)){
  series$returns[i+1] <- (log(series$Open[i+1]) - log(series$Open[i]))
}
 

plot(series$returns, type = "o")

series$indi <- 0
series$indi <- ifelse(series$returns < 0, 0, 1)

series <- na.omit(series)
sum(series$indi)/length(series$indi)


series$y <- 0
for (i in 1:length(series$Open)){
series$y[i] <- (series$indi[i]*series$indi[i+1]) + (1 - series$indi[i])*(1 - series$indi[i+1]) 
}

series <- na.omit(series)
no_seq <- sum(series$y)

CJ_stat <- no_seq/(length(series$returns) - no_seq)

pi<- sum(series$indi)/length(series$indi)
pi_s <- pi^2 + (1- pi)^2




asy_varCJ <- (pi_s * (1 - pi_s) + 2*(pi ^ 3 + (1- pi) ^ 3 - pi_s^2))/(length(series$returns)*(1 - pi_s)^4)
  
  
t_stat <-  (CJ_stat - (pi_s/(1 - pi_s)))/sqrt(asy_varCJ) 
  

  
ifelse(abs(t_stat) >  1.96, print("RW1 is rejected at significance level of 0.05"),
                            print("RW1 cannot be rejected at significance level of 0.05"))
   
  


##########To do:
# 1. Write a code that gives p-values as the output
# 2. Generalize for the all components of DAX30

  
  
  