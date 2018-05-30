# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()


#Simulates a RW
X <- vector(length = 100)
X[1] <- 0


e <- rnorm(n = 100, mean = 0, sd = 1)
Y <- vector(length = 100)

for (i in 1:100) {
  Y[i+1] <-  X[i] + e[i]
  X[i+1] <- Y[i+1]
}

plot(Y, type = "o")
